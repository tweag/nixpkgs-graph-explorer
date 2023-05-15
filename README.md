# nixpkgs-graph-explorer

## Overview

This project aims to make the software dependency graph data from `nixpkgs` more easily accessible for analytics and general exploration use cases.
It does this by extracting the dependency graph for a single `nixpkgs` revision and loading it to a graph database compatible with the [Gremlin](https://tinkerpop.apache.org/gremlin.html) graph traversal language (sqlg + Postgres).

The repository is currently split into a few parts, each with its own documentation:

- `core`: tools to extract the graph of derivations in nixpkgs
- `api`: an API to load and serve the graph
- `web`: a frontend client to interact with the API

## Running locally

To spin up the application locally you can use the Docker Compose stack defined in [docker-compose.yaml](docker-compose.yaml).
This will create a Postgres database, a Gremlin Server which proxies the Postgres database, and the latest version of the `nixpkgs-graph-explorer` API.

```
docker-compose --profile all up
```

The API is available on `localhost:5000` and the front-end at `localhost:9091`.

The graph data is not automatically loaded to the API.
See instructions in [api](./api) to extract and load data.
