package = nixpkgs-graph-explorer

run:
	cabal run $(package)

run-fast:
	cabal run $(package) --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

build:
	cabal build exe:$(package)

build-fast:
	cabal build exe:$(package) --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

install:
	cabal install --overwrite-policy=always
