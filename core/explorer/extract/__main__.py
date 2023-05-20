import copy
import json
import logging
import multiprocessing.pool
import os
import pathlib
import subprocess
import sys
from queue import Empty
from threading import Thread
from typing import IO, Any, Callable, Type

import click

from explorer.core import model


def reader(
    pipe_in: IO[Any],
    queue: multiprocessing.Queue,
    queued_output_paths: set[str],
):
    """Read lines from pipe to push found attribute paths to queue"""
    with pipe_in:
        line: bytes
        # read incoming lines from the Nix expression
        for line in iter(pipe_in.readline, b""):
            s = line.decode()
            if s.startswith("trace: "):
                # remove beginning "trace:"
                s = s[6:]
                try:
                    # FIXME use pydantic
                    parsed_found_derivations = json.loads(s)["foundDrvs"]
                except Exception:
                    # fail silently, most likely some other trace from nixpkgs
                    continue
                # push found derivations to queue if they haven't been queued already
                for found_derivation in parsed_found_derivations:
                    output_path = found_derivation["outputPath"]
                    attribute_path = found_derivation["attributePath"]
                    if output_path not in queued_output_paths:
                        queue.put(attribute_path)
                        queued_output_paths.add(output_path)


def process_attribute_path(
    pipe_out: IO[str],
    queue: multiprocessing.Queue,
    queued_output_paths: set[str],
    visited_output_paths: set[str],
    finder_env: dict,
    attribute_path: str,
    logger: logging.Logger,
):
    """
    Describe a derivation, write results to output pipe and add potential derivations
    to process to queue
    """

    # evaluate value at the attribute path using Nix
    env = copy.deepcopy(finder_env)
    env["TARGET_ATTRIBUTE_PATH"] = attribute_path
    description_process = subprocess.run(
        args=[
            "nix",
            "eval",
            "--extra-experimental-features",
            "nix-command flakes",
            "--json",
            "--file",
            str(pathlib.Path(__file__).parent.joinpath("describe-derivation.nix")),
        ],
        capture_output=True,
        # write to file passed as argument (or stdout)
        env=env,
    )

    description_str = description_process.stdout.decode()
    if len(description_str) == 0:
        logger.warning("Empty evaluation: %s", attribute_path)
        logger.error(description_process.stdout)
        return True

    # write to output pipe
    pipe_out.write(description_str)

    # add its inputs to the queue if they have not been processed
    try:
        description: model.Package = model.Package.parse_raw(description_str)
    except Exception as e:
        logger.warning("Failed to parse to model: attribute_path=%s, str=", attribute_path, description_str)
        logger.error(description_process.stdout)
        raise e from e
    visited_output_paths.add(description.output_path)
    for build_input in description.build_inputs:
        output_path = build_input.output_path
        attribute_path = build_input.attribute_path
        if output_path not in queued_output_paths:
            queue.put(attribute_path)
            queued_output_paths.add(output_path)

    # return success
    return True


def try_or_none(f: Callable, e_type: Type[Exception]):
    try:
        return f()
    except e_type:
        return None


def process_queue(
    output_file_path: str,
    finder_process: subprocess.Popen,
    pool: multiprocessing.pool.ThreadPool,
    queue: multiprocessing.Queue,
    queued_output_paths: set[str],
    visited_output_paths: set[str],
    finder_env: dict,
    logger: logging.Logger,
):
    """Continuously process attribute paths in the queue to the pool"""

    # list of ongoing jobs
    pool_jobs: list[multiprocessing.pool.AsyncResult] = []

    # main loop of task
    while (
        # there is an attribute path to process
        (
            attribute_path := try_or_none(
                lambda: queue.get(block=True, timeout=1),
                Empty,
            )
        )
        is not None
        # there are attribute paths to process
        or not queue.empty()
        # the finder process is still running
        or finder_process.poll() is None
        # there are jobs which have yet to complete
        or len(pool_jobs) > 0
    ):
        # filter jobs to keep uncompleted ones
        for job in pool_jobs:
            if job.ready():
                # job is done, remove it
                pool_jobs.remove(job)
                # check success
                try:
                    job.get()
                except Exception as e:
                    logger.exception(e)

        logger.info(
            "visited=%s,queue=%s,jobs=%s",
            len(visited_output_paths),
            queue.qsize(),
            len(pool_jobs),
        )
        # if no attribute path in the queue, look for the next one
        if attribute_path is None:
            continue

        # process attribute path
        job = pool.apply_async(
            func=process_attribute_path,
            args=[
                output_file_path,
                queue,
                queued_output_paths,
                visited_output_paths,
                finder_env,
                attribute_path,
                logger,
            ],
        )
        pool_jobs.append(job)

    logger.info("QUEUE PROCESSOR CLOSED")


@click.command()
@click.option(
    "--target-flake-ref",
    default="github:nixos/nixpkgs/master",
    help="The reference of the target Nix Flake",
)
@click.option(
    "--target-system",
    default="x86_64-linux",
    help="The system in which to evaluate the packages",
)
@click.option(
    "--n-workers",
    default=1,
    type=int,
    help="Count of workers to spawn to describe the stream of found derivations",
)
@click.option(
    "--verbose",
    "-v",
    is_flag=True,
    help="Increase verbosity",
)
@click.argument(
    "outfile",
    type=click.File("wt"),
)
def extract_data(
    target_flake_ref: str,
    target_system: str,
    n_workers: int,
    verbose: bool,
    outfile: IO[str],
):
    """
    Extract the graph of derivations from a flake as JSON.

    OUTFILE is the path to the output file to write to, use "-" to write to stdout.
    """
    logger = logging.getLogger(__name__)
    if verbose:
        logger.setLevel(logging.INFO)

    # overwrite some environment variables
    extra_env = {
        # required to evaluate some Nixpkgs expressions
        "NIXPKGS_ALLOW_BROKEN": "1",
        "NIXPKGS_ALLOW_INSECURE": "1",
        # arguments to the Nix expression can't be passed with `nix eval`
        # we use environment variables instead
        "TARGET_FLAKE_REF": str(target_flake_ref),
        "TARGET_SYSTEM": str(target_system),
    }
    logger.info(
        "extra_env=%s",
        " ".join(f"{k}={v}" for k, v in extra_env.items()),
    )
    finder_env = os.environ.copy()
    for k, v in extra_env.items():
        finder_env[k] = v

    finder_process = subprocess.Popen(
        args=[
            "nix",
            "eval",
            "--extra-experimental-features",
            "nix-command flakes",
            "--json",
            "--file",
            str(pathlib.Path(__file__).parent.joinpath("find-attribute-paths.nix")),
        ],
        # write to file passed as argument (or stdout)
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=finder_env,
    )

    derivation_description_pool = multiprocessing.pool.ThreadPool(n_workers)
    derivation_description_queue = multiprocessing.Queue()
    queued_output_paths: set[str] = set()
    visited_output_paths: set[str] = set()

    reader_thread = Thread(
        target=reader,
        args=[
            finder_process.stderr,
            derivation_description_queue,
            queued_output_paths,
        ],
    )
    reader_thread.start()

    process_queue_thread = Thread(
        target=process_queue,
        args=[
            outfile,
            finder_process,
            derivation_description_pool,
            derivation_description_queue,
            queued_output_paths,
            visited_output_paths,
            finder_env,
            logger,
        ],
    )
    process_queue_thread.start()

    finder_process.wait()
    logger.info("FINDER EXIT")
    reader_thread.join()
    logger.info("READER THREAD EXIT")
    process_queue_thread.join()
    logger.info("PROCESS QUEUE THREAD EXIT")
    derivation_description_pool.close()
    derivation_description_pool.join()
    logger.info("POOL EXIT")
    is_queue_empty = derivation_description_queue.empty()
    logger.info("IS QUEUE EMPTY: %s", is_queue_empty)
    if not is_queue_empty:
        sys.exit(1)


if __name__ == "__main__":
    logging.basicConfig()
    extract_data()
