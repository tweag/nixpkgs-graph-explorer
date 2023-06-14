from typing import IO

import click

from explorer.core import model


@click.command()
@click.argument(
    "outfile",
    type=click.File("wt"),
)
@click.option(
    "--pretty",
    is_flag=True,
    help="Enable pretty printing of the output JSON Schema",
)
def cli(outfile: IO[str], pretty: bool):
    """
    Write the JSON schema for Derivation to OUTFILE.

    OUTFILE must be a path to a file, or '-' to write to stdout.
    """
    indent = 2 if pretty else None
    schema_json = model.Derivation.schema_json(indent=indent)
    outfile.write(schema_json)


cli()
