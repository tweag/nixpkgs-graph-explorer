# Nixpkgs graph explorer API

The REST API for the `nixpkgs-graph-explorer` project, written using
[FastAPI](https://fastapi.tiangolo.com/).

## Development

To install the application you can run the following from the Nix shell defined
in the project's [flake.nix](../flake.nix):

```bash
poetry install
```

The Nix Flake also provides some helpers for formatting, etc:

```bash
# Formatting
format-python
# Linting
lint-python
```

Tests expect Gremlin Server and Postgres to be available. You can launch these
with the Docker Compose file in the project's root directory:

```bash
# Launches the required docker-compose stack
cd ..
docker-compose --profile db up
```

then

```bash
cd api
poetry run pytest
```

To launch the app with hot reloading for fast iteration, you can use:
```bash
poetry run uvicorn explorer.api:app --reload
```

## Ingesting data

This `nixpkgs-graph-explorer` API comes with a command line utility called
`explorer-ingest-graph` for ingesting a Nix graph JSON file (as specified in the
[core package](../core/nixpkgs-graph.schema.json)). See the command's help menu
for additional details on available options.

```bash
poetry run explorer-ingest-graph --help
```
