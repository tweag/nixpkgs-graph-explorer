# usage:
# TARGET_FLAKE_REF="github:nixos/nixpkgs/master" nix eval --json --file "./nixpkgs-graph.nix"

let
  pkgs = builtins.getFlake "nixpkgs";
  targetFlakeRef = builtins.getEnv "TARGET_FLAKE_REF";
  targetFlakePkgs = if (builtins.getFlake targetFlakeRef).outputs ? packages
                    then
                      (builtins.getFlake targetFlakeRef).outputs.packages.${builtins.currentSystem}
                    else
                      if (builtins.getFlake targetFlakeRef).outputs ? defaultPackage
                      then
                        (builtins.getFlake targetFlakeRef).outputs.defaultPackage.${builtins.currentSystem}
                      else
                        if (builtins.getFlake targetFlakeRef).outputs ? legacyPackages
                        then (builtins.getFlake targetFlakeRef).outputs.legacyPackages.${builtins.currentSystem}
                        else
                          {};
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
          # we don't query pname and version directly because some derivations only have a `name` attribute.
          # use `builtins.parseDrvName` to get the name on all cases
          name = (builtins.tryEval (if okValue ? name then okValue.name else "")).value;
          pname = (builtins.tryEval (if okValue ? name && okValue.name != false
                                     then (builtins.parseDrvName okValue.name).name
                                     else "")).value;
          version = (builtins.tryEval (if okValue ? name && okValue.name != false
                                       then (builtins.parseDrvName okValue.name).version
                                       else "")).value;
          brokenState = (builtins.tryEval (if okValue ? meta.broken then okValue.meta.broken else false)).value;
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
  (mapAttrs (recurse "") targetFlakePkgs)
)
