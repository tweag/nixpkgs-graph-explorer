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

### Organization

The repository is currently split into a few parts, each with its own instructions:

- [core](core): tools to extract the graph of derivations in nixpkgs
- [api](api): an API to load and serve the graph
- [web](web): a frontend client to interact with the API

