# Nixpkgs graph explorer API

* This is the REST API for the `nixpkgs-graph-explorer` project, written using [FastAPI](https://fastapi.tiangolo.com/).
* The project includes a `Dockerfile` and two docker-compose files, `docker-compose.yml` and `docker-compose.prod.yml` for development and production deploys, respectively.
* The application can read environment variables from a `.env` file in the current working directory, if one is provided. In this regard, two basic environment variable files are included in this repository: `.env.dev` and `.env.prod`. These are used by `docker-compose.yml` and `docker-compose.prod.yml` respectively.

## Development

To install the application you can run the following from the Nix shell defined in the project's [flake.nix](../flake.nix):

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

Tests expect Gremlin Server and Postgres to be available. You can launch these with the Docker Compose file in the project's root directory:

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

## Running using Docker

The following is an explanation of how to set up various development and production builds:


### Setting up a development environment with Docker

1. Ensure that `docker` and `docker-compose` are installed.
2. Optionally update environment variables in `.env.dev`
3. `docker-compose up` will build and run the `flask` development server

### Setting up a production environment with Docker

1. Ensure that `docker` and `docker-compose` are installed.
2. Update environment variables in `.env.prod`
3. `docker-compose -f docker-compose.prod.yml up` will build and run a production server using `gunicorn`
