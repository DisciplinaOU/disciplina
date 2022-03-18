{ pkgs ? import ./pkgs.nix }:

let
  project = import ./default.nix { inherit pkgs; };
in
  project.shellFor {
    tools = {
      cabal = "3.2.0.0";
    };

    buildInputs = with pkgs; [
      git
      ephemeralpg
      postgresql
      getopt
      pdf-generator-xelatex
    ];
    exactDeps = true;
  }
