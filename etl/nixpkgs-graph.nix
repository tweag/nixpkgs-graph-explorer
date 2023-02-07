# usage:
# NIXPKGS_FLAKE_REF="github:nixos/nixpkgs/master" nix eval --json --file "./nixpkgs-graph.nix"

let
  nixpkgsFlakeRef = builtins.getEnv "NIXPKGS_FLAKE_REF";
  pkgs = import (builtins.getFlake nixpkgsFlakeRef) { };
in

with pkgs.lib;

let
  recurse =
    parentPath: name: value:
    let
      path = (if parentPath == "" then "" else parentPath + ".") + name;
      valueEvalResult = builtins.tryEval value;
    in
    if valueEvalResult.success then
      let okValue = valueEvalResult.value;
      in
      if isDerivation okValue then
        {
          inherit path;
          # can't name it `outPath` because serialization would only output it instead of dict
          # see Nix `toString` docs
          pname = (builtins.tryEval (if okValue ? pname then okValue.pname else "")).value;
          version = (builtins.tryEval (if okValue ? version then okValue.version else "")).value;
          id = (builtins.tryEval (if okValue ? name then okValue.name else "")).value;
          outputPath =
            let
              pEvalResult = builtins.tryEval (toString okValue);
            in
            if pEvalResult.success then pEvalResult.value
            else null;
          buildInputs =
            map
              (p:
                let
                  pEvalResult = builtins.tryEval (toString p);
                in
                if pEvalResult.success then pEvalResult.value
                else null
              )
              (okValue.buildInputs or [ ]);
          propagatedBuildInputs =
            map
              (p:
                let
                  pEvalResult = builtins.tryEval (toString p);
                in
                if pEvalResult.success then pEvalResult.value
                else null
              )
              (okValue.propagatedBuildInputs or [ ]);
        }
      else
        if isAttrs okValue && (okValue.recurseForDerivations or false) then
          mapAttrs (recurse path) okValue
        else null
    else null;

in

(collect
  (x: x ? outputPath)
  (mapAttrs (recurse "") pkgs)
)
