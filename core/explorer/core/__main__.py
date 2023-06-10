import click

from explorer.core import model


@click.command()
@click.option(
    "--out",
    default="derivation.schema.json",
    help="The output path at which to write the JSON Schema",
    type=click.Path(),
)
@click.option(
    "--stdout",
    default=False,
    help="Write outputs to stdout instead of to a file",
    is_flag=True,
)
@click.option(
    "--pretty/--no-pretty",
    default=True,
    help="Enable/disable pretty printing of the output JSON Schema",
)
def cli(out: str, stdout: bool, pretty: bool):
    if pretty:
        indent = 2
    else:
        indent = None

    schema_json = model.Derivation.schema_json(indent=indent)

    if stdout:
        print(schema_json)
    else:
        click.echo(f"Writing schema to {out}")
        with open(out, "w") as f:
            print(schema_json, file=f)
        click.echo("Done.")


cli()
