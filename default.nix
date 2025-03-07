let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = { };
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install
        ghcid
        haskell-language-server
      ]);
}
