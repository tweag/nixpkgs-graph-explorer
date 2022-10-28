# syntax=docker/dockerfile:1
FROM haskell:8.10-slim
WORKDIR /code/nixpkgs-graph-explorer
RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./nixpkgs-graph-explorer.cabal /code/nixpkgs-graph-explorer/nixpkgs-graph-explorer.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /code/nixpkgs-graph-explorer
RUN	cabal install --install-method=copy --installdir=/opt/
