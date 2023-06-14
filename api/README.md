# `nixpkgs-graph-explorer` -  `api`

An API server to store and serve the data extracted by nixpkgs-graph-explorer core.

## Usage

### Set up

```console
poetry install --only main
```

### Load data

This package comes with a command line utility to load a JSONL file of Nix derivations, where each line follows [`derivation.schema.json`](../core/derivation.schema.json).
To extract such JSONL, see [the `core` package instructions](../core/README.md).

```console
poetry run python -m explorer.api.ingest PATH_TO_FILE
```

or to ingest from stdin:

```console
poetry run python -m explorer.api.ingest -
```

## Development

### Set up

Use the Nix development shell provided in this repository, see `../README.md#use-nix`

```console
poetry install
```

The API server expects Gremlin Server and Postgres to be available.
You can launch these with the Docker Compose file in the project's root directory:

```console
# From the repository root folder
docker-compose --profile db up
```

### Run locally

To launch the app with hot reloading for fast iteration, use:

```console
poetry run uvicorn explorer.api:app --port 5000 --reload
```

### Test

The project uses pytest:

```
poetry run pytest
```

