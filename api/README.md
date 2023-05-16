# `nixpkgs-graph-explorer` -  `api`

An API server to store and serve the data extracted by nixpkgs-graph-explorer core.

## Usage

### Set up

```
poetry install
```

### Load data

This package comes with a command line utility called `explorer-ingest-graph` to load a Nix graph JSON file that follows [`nixpkgs-graph.schema.json`](../core/nixpkgs-graph.schema.json).
To extract such JSON, see [the `core` package instructions](../core/README.md).

```console
poetry run python -m explorer.api.ingest --graph-json PATH_TO_FILE
```

## Development

### Set up

Use the Nix development shell provided in this repository, see `../README.md#use-nix`

```console
poetry install
```

### Run locally

To launch the app with hot reloading for fast iteration, use:

```console
poetry run uvicorn explorer.api:app --reload
```

### Test

Tests expect Gremlin Server and Postgres to be available.
You can launch these with the Docker Compose file in the project's root directory:

```console
# Launches the required docker-compose stack
# From the repository root folder
docker-compose --profile db up
```

The project uses pytest:

```
poetry run pytest
```

