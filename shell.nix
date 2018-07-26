{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  hedgehog-inline-java-testing = import ./. {};
  addDevTools = drv: pkgs.haskell.lib.addBuildDepends drv [ ];

  in (addDevTools hedgehog-inline-java-testing).env
