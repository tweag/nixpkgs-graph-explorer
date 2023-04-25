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

  # utility function for safe evaluation, null if fails
  safeEval = v: (builtins.tryEval v).value or null;

  # utility specific to nixpkgs, as nixpkgs prevents computing outPath if meta.platforms does not contain target system
  safeNixpkgsOutPathEval =
    drv:
    if !(builtins.elem targetSystem (drv.meta.platforms or [ targetSystem ]))
    then null
    else safeEval drv.outPath;
in

let
  recurse =
    parentPath: name: value:
    let
      # evaluation path in attributes tree
      attributesPath = if name == null then null else (if parentPath == "" then "" else parentPath + ".") + name;
      okValue = safeEval value;
    in
    if !(nixpkgs.lib.isDerivation okValue)
    then
      if (nixpkgs.lib.isAttrs okValue) && (okValue.recurseForDerivations or false)
      then
        builtins.mapAttrs (recurse attributesPath) okValue
      else
        null
    else
      let
        name = (builtins.tryEval okValue.name).value or null;
        # nixpkgs refuses to evaluate outPath when the platform is same as current
        # we need to check it
        platforms = okValue.meta.platforms or [ targetSystem ];
        isRightPlatform = builtins.elem targetSystem platforms;
      in
      if !(nixpkgs.lib.isString name) then null else
      {
        inherit name;
        parsedName = (builtins.parseDrvName name);
        nixpkgs_metadata =
            {
              pname = (builtins.tryEval (if okValue ? pname
                                         then okValue.pname
                                         else false)).value or null;
              version = (builtins.tryEval (if okValue ? version
                                           then okValue.version
                                           else "")).value;
              broken = (builtins.tryEval (if okValue ? meta.broken
                                          then okValue.meta.broken
                                          else false)).value;
              license = (builtins.tryEval (if okValue ? meta.license.fullName
                                           then okValue.meta.license.fullName
                                           else "")).value;
            };

        # path to use to get to the derivation in the flake packages
        # e.g. "foo.bar" means you can get this derivation using "flake#foo.bar"
        inherit attributesPath;

        # path to the evaluated derivation file
        derivationPath = if isRightPlatform then safeEval (okValue.drvPath) else null;

        # path to the realized (=built) derivation
        # note: we can't name it `outPath` because serialization would only output it instead of dict, see Nix `toString` docs
        outputPath = safeNixpkgsOutPathEval okValue;
        output_paths =
          let output_paths = okValue.outputs or [ ];
          in map (name: { inherit name; path = safeNixpkgsOutPathEval okValue.${name}; }) output_paths;
        buildInputs = map (recurse attributesPath null) (okValue.buildInputs or [ ]);
        propagatedBuildInputs = map (recurse attributesPath null) (okValue.propagatedBuildInputs or [ ]);

      }
  ;
in

{
  packages =
    map
      (drv: {
        inherit (drv) name parsedName attributesPath derivationPath outputPath output_paths nixpkgs_metadata;
        build_inputs = builtins.concatLists
          [(nixpkgs.lib.remove null
            (map (buildInputDrv:
              if buildInputDrv ? outputPath && buildInputDrv.outputPath != null
              then
                {
                  build_input_type = "build_input";
                  package_output_path = buildInputDrv.outputPath;
                }
              else null)
              (drv.buildInputs or [])))
           (nixpkgs.lib.remove null
             (map (buildInputDrv:
               if buildInputDrv ? outputPath && buildInputDrv.outputPath != null
               then
                 {
                   build_input_type = "propagated_build_input";
                   package_output_path = buildInputDrv.outputPath;
                 }
               else null)
               (drv.propagatedBuildInputs or [])))
          ];
      }
      )
      (nixpkgs.lib.collect
        (x: (x.derivationPath or null) != null)
        (builtins.mapAttrs (recurse "") targetFlakePkgs)
      );
  }
