# usage:
# NIXPKGS_FLAKE_REF="github:nixos/nixpkgs/master" nix eval --json --file "./nixpkgs-graph.nix"

let
  nixpkgsFlakeRef = builtins.getEnv "NIXPKGS_FLAKE_REF";
  pkgs = import (builtins.getFlake nixpkgsFlakeRef) {
    config = {
      # package 'python-2.7.18.6' is marked as insecure
      # we need to allow this package otherwise it is refusing to evaluate
      permittedInsecurePackages = [
        "python-2.7.18.6"
      ];
    };
  };
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
          name = (builtins.tryEval (if okValue ? name then okValue.name else "")).value;
          brokenState = (builtins.tryEval (if okValue ? meta.broken then okValue.meta.broken else "")).value;
          license = (builtins.tryEval (if okValue ? meta.license.fullName then okValue.meta.license.fullName else "")).value;
          outputPath =
            let
              pEvalResult = builtins.tryEval (toString okValue);
            in
            if pEvalResult.success then pEvalResult.value
            else null;
          outputPathAll =
            let
              outputs =
                let
                  pEvalResult = builtins.tryEval (toString (if okValue ? outputs then okValue.outputs else ""));
                in
                if pEvalResult.success then splitString " " pEvalResult.value
                else [];
            in
            map
              (p:
                { name = p; path = (builtins.tryEval (if okValue ? ${p} then toString okValue.${p} else "")).value; }
              )
              (outputs);
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
