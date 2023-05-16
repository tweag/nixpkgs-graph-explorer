# `nixpkgs-graph-explorer` -  `core`

Core data model used for extracting Nix dependencies and related utilities.

This package
- defines [a schema](./nixpkgs-graph.schema.json) for a graph of derivations of nixpkgs.
- provides a command line tool to extract corresponding data

## Usage

### Set up

```console
poetry install
```

### Extract nixpkgs graph of derivations

To extract the data from nixpkgs, use:

```console
poetry run python -m explorer.extract graph.json
```

To write to stdout, use `-` instead of a file path

```console
poetry run python -m explorer.extract -
```

You can use `--help` to see the tool documentation.

## Development

### Set up

Use the Nix development shell provided in this repository, see `../README.md#use-nix`

```console
poetry install
```

### JSON schema

The JSON schema is written as a `pydantic` model in `explorer.core`.

Once modified, update the JSON schema using the command:

```console
poetry run python -m explorer.core
```

