{ pkgs ? import (
  builtins.fetchGit {
  name = "nixpkgs-unstable-2025-02-02";
  url = "https://github.com/nixos/nixpkgs/";
  ref = "refs/heads/nixpkgs-unstable";
  # Commit hash for nixpkgs-unstable as of 2025-02-02
  rev = "9189ac18287c599860e878e905da550aa6dec1cd";
}) {} }:
let
  bin = pkgs.haskell.lib.justStaticExecutables (pkgs.haskellPackages.callCabal2nix "entrance-swap" ./. {});
in pkgs.dockerTools.buildLayeredImage {
  name = "entrance_swap";
  tag = "latest";
  contents = [ bin ];
  config.Entrypoint = [ "${bin}/bin/entrance-swap" ];
}
