{ nixpkgs ? builtins.getFlake "nixpkgs" }:

rec {
  /* Utility function to enumerate a list
  Type: [a] -> [(int, a)]
  
  Example:
      enumerate [ "a" "b" ]
      => [ { index = 0; value = "a"; } { index = 1; value = "b"; } ]
  */
  enumerate = lst: map (zippedElem: { index = zippedElem.fst; value = zippedElem.snd; }) (nixpkgs.lib.zipLists (nixpkgs.lib.range 0 (builtins.length lst)) lst);

  /* A modified version of nixpkgs lib.attrsets.collect to go inside lists as well
  */
  collect =
    pred:
    v:
    if pred v then
      [ v ]
    else if nixpkgs.lib.isAttrs v then
      nixpkgs.lib.concatMap (collect pred) (nixpkgs.lib.attrValues v)
    else if nixpkgs.lib.isList v then
      nixpkgs.lib.concatMap (collect pred) v
    else
      [];

  /* Packages in a flake are usually a flat attribute set in outputs, but legacy systems use `legacyPackages`
  */
  getFlakePkgs = flake: targetSystem: flake.outputs.packages.${targetSystem} or flake.outputs.defaultPackage.${targetSystem} or flake.outputs.legacyPackages.${targetSystem} or { };

  /* Utility function for safe evaluation of any value, null if evaluation fails
  */
  safeEval = v: (builtins.tryEval v).value or null;

  /* Utility specific to nixpkgs, as nixpkgs prevents computing some fields if meta.platforms does not contain the target system
  Args:
      targetSystem: (string) target system
      f: (derivation -> a) function to apply to derivation
      drv: derivation
  */
  safePlatformDrvEval =
    targetSystem: f: drv:
    if !(builtins.elem targetSystem (drv.meta.platforms or [ targetSystem ]))
    then null
    else safeEval (f drv);
}
