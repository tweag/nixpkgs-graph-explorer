# nixpkgs-graph-explorer

## Overview

This project aims to make the software dependency graph data from `nixpkgs` more easily accessible for analytics and general exploration use cases.

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

To fill it, run the extraction using the `core` component:

```console
cd core
poetry run python -m explorer.extract ../derivations.jsonl
```

Then load this data using the `api` component:

```console
cd api
poetry run python -m explorer.api.ingest ../derivations.jsonl
```

### Repository structure

This repository is a monorepo, each component in its folder.

- [core](./core): tools to extract the graph of derivations in nixpkgs
- [api](./api): tools to load the extracted graph in a database and serve it with an API
- [web](./web): a frontend client to interact with the API and explore the graph viusally

