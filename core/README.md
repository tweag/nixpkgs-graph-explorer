# `nixpkgs-graph-explorer` -  `core`

Core data model used for extracting Nix dependencies and related utilities.

This package
- defines [a schema](./nixpkgs-graph.schema.json) for a graph of derivations of nixpkgs.
- provides a command line tool to extract corresponding data

## Usage

### Extract nixpkgs graph of derivations

To extract the data from nixpkgs, use:

```bash
poetry run python -m explorer.extract -
```

You can use `--help` to see the tool documentation.

## Development

### Set up

To install the application you can run the following from the Nix shell defined in the project's [flake.nix](../flake.nix):

```bash
poetry install
```

Then launch [vscode](https://code.visualstudio.com/) or your favorite editor from poetry,
so that it picks up dependencies:

```bash
poetry run code .
```

### JSON schema

The JSON schema is written as a `pydantic` model in `explorer.core`.

Once modified, update the JSON schema using the command:

```bash
poetry run python -m explorer.core
```

