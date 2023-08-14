# `nixtract`

A CLI tool to extract the graph of derivations from a Nix flake.

## Usage

### Set up

```console
pip install git+https://github.com/tweag/nixtract.git
```

### Extract nixpkgs graph of derivations

To extract the data from a specific nixpkgs, use:

```console
nixtract ./derivations.jsonl
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

