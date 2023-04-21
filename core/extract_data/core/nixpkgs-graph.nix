# usage:
# NIXPKGS_ALLOW_BROKEN=1 NIXPKGS_ALLOW_INSECURE=1 TARGET_FLAKE_REF="github:nixos/nixpkgs/master" TARGET_SYSTEM="x86_64-linux" nix eval --json --file "./nixpkgs-graph.nix"

let
  nixpkgs = builtins.getFlake "nixpkgs";
  targetFlakeRef = builtins.getEnv "TARGET_FLAKE_REF";
  # We specify the system of the target here to avoid ambiguity
  # The outputs of a flake will produce different results on every system
  targetSystem = builtins.getEnv "TARGET_SYSTEM";
  targetFlakePkgs =
    let
      flake = builtins.getFlake targetFlakeRef;
    in
      flake.outputs.packages.${targetSystem} or flake.outputs.defaultPackage.${targetSystem} or flake.outputs.legacyPackages.${targetSystem} or { };
in

with nixpkgs.lib;

let
  recurse =
    name: value:
    let
      # path = (if parentPath == "" then "" else parentPath + ".") + name;
      valueEvalResult = builtins.tryEval value;
    in
    if valueEvalResult.success then
      let okValue = valueEvalResult.value;
      in
      if isDerivation okValue then
        {
          # inherit path;
          # can't name it `outPath` because serialization would only output it instead of dict
          # see Nix `toString` docs
          # we don't query pname and version directly because some derivations only have a `name` attribute.
          # use `builtins.parseDrvName` to get the name on all cases

          name = (builtins.tryEval (if okValue ? name then okValue.name else "")).value;
          output_paths =
            let
              outputs =
                let
                  pEvalResult = builtins.tryEval (toString (if okValue ? outputs
                                                            then okValue.outputs else ""));
                in
                if pEvalResult.success then splitString " " pEvalResult.value
                else [ ];
            in
            map
              (p:
                { name = p;
                  path = (builtins.tryEval (if okValue ? ${p}
                                            then toString okValue.${p} else "")).value; }
              )
              (outputs);
          nixpkgs_metadata =
            {
              pname = (builtins.tryEval (if okValue ? name && okValue.name != false
                                         then (builtins.parseDrvName okValue.name).name
                                         else "")).value;
              version = (builtins.tryEval (if okValue ? name && okValue.name != false
                                           then (builtins.parseDrvName okValue.name).version
                                           else "")).value;
              broken = (builtins.tryEval (if okValue ? meta.broken
                                          then okValue.meta.broken
                                          else false)).value;
              license = (builtins.tryEval (if okValue ? meta.license.fullName
                                           then okValue.meta.license.fullName
                                           else "")).value;
            };
          drvPath = (builtins.tryEval (if okValue ? drvPath then okValue.drvPath else "")).value;
          buildInputs =
            map
              (p:
                let
                  pEvalResult = builtins.tryEval (if p ? drvPath then p.drvPath else null);
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
          build_inputs = [ ];
        }
      else
        null
    else null;

in

{
  packages =
    builtins.attrValues
    (filterAttrs
      (n: v: v != null)
      (mapAttrs (recurse) targetFlakePkgs)
    );
}
