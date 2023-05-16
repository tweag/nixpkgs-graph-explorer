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

We use [Nix](https://nixos.org/) to manage dependencies, so you need
[Nix: the package manager](https://nixos.org/download.html) installed to develop
nixpkgs-graph-explorer. Also, because we use advanced features of Nix (such as flakes), we
recommend you turn them ON by default with an alias (in `.bashrc` for example):

```
alias nix='nix --extra-experimental-features nix-command --extra-experimental-features flakes'
```

The repository is currently split into a few parts, each with its own instructions:

- [core](core): tools to extract the graph of derivations in nixpkgs
- [api](api): an API to load and serve the graph
- [web](web): a frontend client to interact with the API

