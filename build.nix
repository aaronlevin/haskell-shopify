{ nixpkgs ? (import <nixpkgs> {})
, haskell ? (import <nixpkgs> {}).haskellPackages }:
let
  shopify = with haskell; {
    lib = callPackage ./cabal.nix { };
  };
in rec {
  haskellShopifyBuildEnv = nixpkgs.myEnvFun {
    name = "haskell-shopify-build";

    buildInputs = [
      shopify.lib
    ];
  };
}
