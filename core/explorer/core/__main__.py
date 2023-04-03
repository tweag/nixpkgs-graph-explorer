from explorer.core import model

import click


@click.command()
@click.option(
    "--out",
    default="nixpkgs-graph.schema.json",
    help="The output path at which to write the JSON Schema",
    type=click.Path(),
)
@click.option(
    "--pretty/--no-pretty",
    default=True,
    help="Enable/disable pretty printing of the output JSON Schema",
)
def cli(out: str, pretty: bool):
    click.echo(f"Writing schema to {out}")
    if pretty:
        indent = 2
    else:
        indent = None
    with open(out, "w") as f:
        print(model.NixGraph.schema_json(indent=indent), file=f)
    click.echo("Done.")


cli()
