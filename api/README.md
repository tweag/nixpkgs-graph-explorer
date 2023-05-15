# `nixpkgs-graph-explorer` -  `api`

An API server to store and serve the data extracted by nixpkgs-graph-explorer core.

## Usage

### Load data

This package comes with a command line utility called `explorer-ingest-graph` to load a Nix graph JSON file that follows [`nixpkgs-graph.schema.json`](../core/nixpkgs-graph.schema.json).
To extract such JSON, see [the `core` package instructions](../core/README.md).

```bash
poetry run python -m explorer.api.ingest --graph-json PATH_TO_FILE
```

## Development

### Set up

To install the application you can run the following from the Nix shell defined in the project's [flake.nix](../flake.nix):

```bash
poetry install
```

### Run locally

To launch the app with hot reloading for fast iteration, use:

```bash
poetry run uvicorn explorer.api:app --reload
```

### Test

Tests expect Gremlin Server and Postgres to be available.
You can launch these with the Docker Compose file in the project's root directory:

```bash
# Launches the required docker-compose stack
# From the repository root folder
docker-compose --profile db up
```

The project uses pytest:

```
poetry run pytest
```

