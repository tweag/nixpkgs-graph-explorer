import copy
import json
import logging
import multiprocessing.pool
import os
import pathlib
import subprocess
import sys
from queue import Empty
from threading import Thread, Lock
from typing import IO, Any, Callable, Type

import click

from explorer.core import model


def finder_output_reader(
    pipe_in: IO[Any],
    queue: multiprocessing.Queue,
    queued_output_paths: set[str],
    logger: logging.Logger,
):
    """Read lines from finder standard output to push found attribute paths to queue"""
    with pipe_in:
        line_bytes: bytes
        # read incoming lines
        for line_bytes in iter(pipe_in.readline, b""):
            line: str = line_bytes.decode()

            if not line.startswith("trace: "):
                sys.stderr.write(line)
                continue

            # remove beginning "trace:"
            trace = line[6:]

            try:
                # FIXME use pydantic
                parsed_found_derivations: list[dict[str, str]] = json.loads(trace)[
                    "foundDrvs"
                ]
            except Exception:
                # fail silently, most likely some other trace from nixpkgs
                sys.stderr.write(line)
                continue

            # push found derivations to queue if they haven't been queued already
            for found_derivation in parsed_found_derivations:
                attribute_path = found_derivation.get("attributePath")
                output_path = found_derivation.get("outputPath")
                if attribute_path is None or output_path is None:
                    logger.warning("Wrong derivation: %s", json.dumps(found_derivation))
                    continue
                if output_path not in queued_output_paths:
                    queue.put(attribute_path)
                    queued_output_paths.add(output_path)


def process_attribute_path(
    pipe_out: IO[str],
    pipe_out_lock: Lock,
    queue: multiprocessing.Queue,
    queued_output_paths: set[str],
    visited_output_paths: set[str],
    finder_env: dict,
    offline: bool,
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
            arg
            for arg in [
                "nix",
                "eval",
                "--offline" if offline else None,
                "--extra-experimental-features",
                "nix-command flakes",
                "--json",
                "--file",
                str(pathlib.Path(__file__).parent.joinpath("describe-derivation.nix")),
            ]
            if arg is not None
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

    # add its inputs to the queue if they have not been processed
    try:
        description: model.Package = model.Package.parse_raw(description_str)
    except Exception as e:
        logger.warning(
            "Failed to parse to model: attribute_path=%s, str=",
            attribute_path,
            description_str,
        )
        logger.error(description_process.stdout)
        raise e from e

    if description.output_path is None:
        logger.warning("Ignore empty outputPath: %s", attribute_path)
        return True

    # write to output pipe
    with pipe_out_lock:
        pipe_out.write(description.json(by_alias=False))
        pipe_out.write("\n")

    visited_output_paths.add(description.output_path)
    for build_input in description.build_inputs:
        output_path = build_input.output_path
        attribute_path = build_input.attribute_path
        if output_path is not None and output_path not in queued_output_paths:
            queue.put(attribute_path)
            queued_output_paths.add(output_path)

    # return success
    return True


def try_or_none(f: Callable, e_type: Type[Exception]):
    try:
        return f()
    except e_type:
        return None


def queue_processor(
    outfile: IO[str],
    outfile_lock: Lock,
    finder_process: subprocess.Popen,
    pool: multiprocessing.pool.ThreadPool,
    queue: multiprocessing.Queue,
    queued_output_paths: set[str],
    visited_output_paths: set[str],
    finder_env: dict,
    offline: bool,
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
                outfile,
                outfile_lock,
                queue,
                queued_output_paths,
                visited_output_paths,
                finder_env,
                offline,
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
    "--offline",
    is_flag=True,
    help="Pass --offline to Nix commands",
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
def cli(
    target_flake_ref: str,
    target_system: str,
    n_workers: int,
    offline: bool,
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

    # start process to find derivations directly available
    finder_process = subprocess.Popen(
        args=[
            arg
            for arg in [
                "nix",
                "--offline" if offline else None,
                "eval",
                "--extra-experimental-features",
                "nix-command flakes",
                "--json",
                "--file",
                str(pathlib.Path(__file__).parent.joinpath("find-attribute-paths.nix")),
            ]
            if arg is not None
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=finder_env,
    )

    # while we find these derivations directly available, we process found derivations
    # at the same time to describe them, but also to find derivations indirectly
    # available via their build inputs
    derivation_description_pool = multiprocessing.pool.ThreadPool(n_workers)

    # we can't access the pool job queue so we use our own (thanks encapsulation U_U)
    derivation_description_queue = multiprocessing.Queue()
    # we don't want to process the same derivation twice, so we use their output path to
    # check that
    queued_output_paths: set[str] = set()
    visited_output_paths: set[str] = set()

    # we need to use a mutex lock to make sure we write one line at a time
    outfile_lock = Lock()

    # read derivations found by the finder to feed the processing queue
    reader_thread = Thread(
        target=finder_output_reader,
        args=[
            finder_process.stderr,
            derivation_description_queue,
            queued_output_paths,
            logger,
        ],
    )
    reader_thread.start()

    # process derivations pushed to the processing queue
    process_queue_thread = Thread(
        target=queue_processor,
        args=[
            outfile,
            outfile_lock,
            finder_process,
            derivation_description_pool,
            derivation_description_queue,
            queued_output_paths,
            visited_output_paths,
            finder_env,
            offline,
            logger,
        ],
    )
    process_queue_thread.start()

    # all is done once finder is done, reader is done and processing queue is done
    finder_process.wait()
    logger.info("FINDER EXIT")
    reader_thread.join()
    logger.info("READER THREAD EXIT")
    process_queue_thread.join()
    logger.info("PROCESS QUEUE THREAD EXIT")
    derivation_description_pool.close()
    derivation_description_pool.join()
    logger.info("POOL EXIT")

    if not derivation_description_queue.empty():
        logger.error("Finished but queue is not empty")
        sys.exit(1)


if __name__ == "__main__":
    logging.basicConfig()
    cli()
