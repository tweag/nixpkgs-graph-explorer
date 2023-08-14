# nixpkgs-graph-explorer

## Overview

This project aims to make the software dependency graph data from `nixpkgs` more easily accessible for analytics and general exploration use cases.

## UPDATE 2023-08-14

The project is moving to a focus on the extraction part first.
See [`nixtract](https://github.com/tweag/nixtract) which focuses on the `core` component of this project.

## Running locally

```console
docker-compose --profile all up
```

The web UI is available at [http://localhost:9091](http://localhost:9091).

The graph data is not automatically loaded.
See instructions in [api](./api) to load data.

<!-- TODO add instructions to load data here -->

## Development

### Set up

#### Using Nix

[Nix](https://nixos.org/) helps manage development dependencies.
You can get it [here](https://nixos.org/download.html).

Make sure that you have `~/.config/nix/nix.conf` with the following:
```
experimental-features = nix-command flakes
```

To enter the Nix development shell, use this command from the root folder of the repository:

```console
nix develop
```

### Start all services in development

1. Start the database
   ```console
   docker-compose --profile db up
   ```
2. Start the API
   ```console
   cd api
   poetry install
   poetry run uvicorn explorer.api:app --port 5000 --reload
   ```
3. Start the web UI
   ```console
   cd web
   npm install
   npm run dev
   ```

For more details read each of [`api/README.md`](./api/README.md) and [`web/README.md`](./web/README.md).

### Extract and ingest

The first time you run the app locally, the database is empty.

To fill it, you have two options: using Docker or running the commands directly.

#### Using Docker

To extract and ingest the data using Docker, follow these steps:

1. Build the Docker image:

```console
docker build -f docker/etl/Dockerfile -t nixpkgs-graph-explorer-etl .
```

2. Run the Docker container to execute the extract and ingest commands:
```console
docker run -it --rm --network host nixpkgs-graph-explorer-etl
```

#### Running the commands directly

If you prefer to run the commands directly without Docker, follow these steps:

1. Run the extraction using the `core` component:

```console
cd core
poetry run python -m explorer.extract ../derivations.jsonl
```

2. Load this data using the `api` compnent:

```console
cd api
poetry run python -m explorer.api.ingest ../derivations.jsonl
```

### Repository structure

This repository is a monorepo, each component in its folder.

- [core](./core): tools to extract the graph of derivations in nixpkgs
- [api](./api): tools to load the extracted graph in a database and serve it with an API
- [web](./web): a frontend client to interact with the API and explore the graph visually
