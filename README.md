# nixpkgs-graph-explorer

> This project is currently in a prototype state.

## Overview

This project aims to make the software dependency graph data from `nixpkgs` more
easily accessible for analytics and general exploration use cases. It does this
by extracting the dependency graph for a single `nixpkgs` revision and loading
it to a graph database compatible with the
[Gremlin](https://tinkerpop.apache.org/gremlin.html) graph traversal language
(sqlg + Postgres).

The repository is currently split into a few parts, each with its own
documentation:

* `api` The REST API for serving the graph
* `etl` Scripts for extracting the nixpkgs graph and loading it to Gremlin
  Server
* `config` Various configuration
* `docker` Docker image definitions, etc.
* `web` Web app to interact with the API

## Running locally

To spin up the application locally you can use the Docker Compose stack defined
in [docker-compose.yaml](docker-compose.yaml). This will create a Postgres
database, a Gremlin Server which proxies the Postgres database, and the latest
version of the `nixpkgs-graph-explorer` API.

We currently divide the services in `docker-compose.yaml` into several profiles:

| profile  | services                              |
|----------|---------------------------------------|
| `db`       | Gremlin Server, Postgres              |
| `backend`  | Gremlin Server, Postgres, API         |
| `frontend` | Web UI                                |
| `all`      | Gremlin Server, Postgres, API, Web UI |


First, you'll need to build the images:

```
docker-compose --profile all build
```

Then, launch the stack:

```
docker-compose --profile all up
```

nixpkgs-graph-explorer is now running. The API is available on `localhost:5000`
and the front-end at `localhost:9091`, However, no data is currently loaded to
the database. See the instructions in [api](./api) on how to ingest data.
