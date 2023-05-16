# nixpkgs-graph-explorer

## Overview

This project aims to make the software dependency graph data from `nixpkgs` more easily accessible for analytics and general exploration use cases.

## Running locally

```
docker-compose --profile all up
```

The web UI is available at [http://localhost:9091](http://localhost:9091).

The graph data is not automatically loaded.
See instructions in [api](./api) to load data.

<!-- TODO add instructions to load data here -->

## Development

The repository is currently split into a few parts, each with its own instructions:

- [core](core): tools to extract the graph of derivations in nixpkgs
- [api](api): an API to load and serve the graph
- [web](web): a frontend client to interact with the API

