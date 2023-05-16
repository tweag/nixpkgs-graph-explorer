# `nixpkgs-graph-explorer` - `web`

A web UI to interact with the API.

## Development

The client is build with [Lit](https://lit.dev/) and [Shoelace](https://shoelace.style/)

### Set up

Use the Nix development shell provided in this repository, see `../README.md#use-nix`

```
npm install
```

### Run locally

```
npm run dev
```

The client expects the API to be running at `http://localhost:5000`.

You can configure a different URL in `vite.config.ts`
