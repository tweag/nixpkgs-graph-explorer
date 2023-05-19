# Describe a derivation in the NixGraph core model
#
# Args (as environment variables):
#     TARGET_FLAKE_REF: flake reference to evaluate
#     TARGET_SYSTEM: system to evaluate
#     TARGET_ATTRIBUTE_PATH: attribute path to the derivation to evaluate
#
# Example:
# TARGET_FLAKE_REF="nixpkgs" TARGET_SYSTEM="x86_64-linux" TARGET_ATTRIBUTE_PATH="python3" nix eval --json --file describe-derivation.nix

let
  nixpkgs = builtins.getFlake "nixpkgs";
  lib = import ./lib.nix { inherit nixpkgs; };

  # Arguments have to be taken from environment when using `nix` command
  targetFlakeRef = builtins.getEnv "TARGET_FLAKE_REF";
  targetSystem = builtins.getEnv "TARGET_SYSTEM";
  targetAttributePath = builtins.getEnv "TARGET_ATTRIBUTE_PATH";

  # Get pkgs
  targetFlake = builtins.getFlake targetFlakeRef;
  targetFlakePkgs = lib.getFlakePkgs targetFlake targetSystem;

  # Follow attribute path (split by dot) to access value in tree of nested attribute sets and lists
  getAttrByPath = collection: pathList:
    let
      x = builtins.head pathList;
      value =
        if nixpkgs.lib.isAttrs collection
        then collection.${x}
        else
          if nixpkgs.lib.isList collection
          then
            let index = nixpkgs.lib.toIntBase10 x;
            in builtins.elemAt collection index
          else builtins.throw "Trying to follow path in neither an attribute set nor a list";
    in
    if builtins.length pathList > 1 then
      let
        # need to skip an item, see `builtins.split` doc
        xs = builtins.tail (builtins.tail pathList);
      in
      getAttrByPath value xs
    else value;
  value =
    let splitPath = (builtins.split "\\." targetAttributePath);
    in getAttrByPath targetFlakePkgs splitPath;

in
{
  name = value.name;
  parsed_name = (builtins.parseDrvName value.name);
  attributePath = targetAttributePath;
  nixpkgsMetadata =
    {
      pname = (builtins.tryEval (if value ? pname then value.pname else false)).value or null;
      version = (builtins.tryEval (if value ? version then value.version else "")).value;
      broken = (builtins.tryEval (if value ? meta.broken then value.meta.broken else false)).value;
      license = (builtins.tryEval (if value ? meta.license.fullName then value.meta.license.fullName else "")).value;
    };

  # path to the evaluated derivation file
  derivationPath = lib.safePlatformDrvEval targetSystem (drv: drv.drvPath) value;

  # path to the realized (=built) derivation
  # note: we can't name it `outPath` because serialization would only output it instead of dict, see Nix `toString` docs
  outputPath = lib.safePlatformDrvEval targetSystem (drv: drv.outPath) value;
  outputPaths = map (name: { inherit name; path = lib.safePlatformDrvEval targetSystem (drv: drv.outPath) value.${name}; }) value.outputs;
  inputs = builtins.listToAttrs
    (map
      (inputType: {
        name = inputType;
        value = map
          (elem:
            if builtins.isPath elem.value then
              {
                attributePath = targetAttributePath + ".${inputType}.${builtins.toString elem.index}";
                outputPath = elem.value;
              }
            else
              let inputDrv = elem.value;
              in
              {
                attributePath = targetAttributePath + ".${inputType}.${builtins.toString elem.index}";
                outputPath = lib.safePlatformDrvEval targetSystem (drv: drv.outPath) inputDrv;
              })
          (lib.enumerate (value.${inputType} or [ ]));
      })
      [ "nativeBuildInputs" "buildInputs" "propagatedBuildInputs" ]
    );
}

