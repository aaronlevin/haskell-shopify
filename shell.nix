{ nixpkgs ? (import <nixpkgs> {})
, haskell ? (import <nixpkgs>{}).haskellPackages }:
let
in rec {
  haskellShopifyEnv = nixpkgs.myEnvFun {
    name = "haskell-shopify";

    buildInputs = [
      # build depends
      haskell.aeson
      haskell.comonad
      haskell.conduit
      haskell.dataDefault
      haskell.exceptions
      haskell.free
      haskell.httpClient
      haskell.httpClientTls
      haskell.httpConduit
      haskell.httpTypes
      haskell.mtl
      haskell.resourcet
      haskell.scientific
      haskell.text
      haskell.time
      haskell.unorderedContainers

      # test depends
      haskell.HUnit
      haskell.QuickCheck
      haskell.rawStringsQq
      haskell.testFramework
      haskell.testFrameworkHunit
      haskell.testFrameworkQuickcheck2

      # editor tooling
      haskell.ghc
      haskell.ghcMod
      haskell.cabalInstall
      nixpkgs.vimHugeX

      # random / delete
      haskell.transformers
    ];
  };
}
