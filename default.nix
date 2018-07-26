{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  addBuildDeps = drv:
      pkgs.haskell.lib.addBuildDepends drv [ pkgs.openjdk10 pkgs.gradle ];

  in addBuildDeps
     (variant (haskellPackages.callPackage (import ./hedgehog-inline-java-testing.nix) {}))
