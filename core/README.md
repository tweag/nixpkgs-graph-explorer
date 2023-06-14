# `nixpkgs-graph-explorer` -  `core`

Core data model used for extracting Nix dependencies and related utilities.

This package provides a command line tool to extract derivations from a Nix flake.

Result of this extraction is a JSONL file, each line complying to [`./derivation.schema.json`](./derivation.schema.json).

## Usage

### Set up

```console
poetry install --only main
```

### Extract nixpkgs graph of derivations

To extract the data from nixpkgs, use:

```console
poetry run python -m explorer.extract derivations.json
```

To write to stdout, use `-` instead of a file path

```console
poetry run python -m explorer.extract -
```

To learn more about the available options:

```console
poetry run python -m explorer.extract --help
```

## Development

### Set up

Use the Nix development shell provided in this repository, see `../README.md#use-nix`

```console
poetry install
```

### JSON schema

The JSON schema is written as a [`pydantic`](https://docs.pydantic.dev/latest/) model in `explorer.core`.

Once modified, update the JSON schema using the command:

```console
poetry run python -m explorer.core ./derivation.schema.json
```

