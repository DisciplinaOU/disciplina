let
  overlay = import ''${builtins.fetchGit "ssh://git@github.com:/serokell/serokell-overlay.git"}/pkgs'';
  nixpkgs = import (builtins.fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
    overlays = [ overlay ];
  };  
in

with nixpkgs;
with haskell.lib;

let
  appendBuildFlag = drv: x: overrideCabal drv (drv: { buildFlags = (drv.buildFlags or []) ++ [x]; });
in

buildStackApplication {
  package = "disciplina";
  src = lib.cleanSource ./.;
  ghc = haskell.compiler.ghc822;

  overrides = final: previous: {
    rocksdb-haskell = dependCabal previous.rocksdb-haskell [ rocksdb ];
    cardano-sl-networking = appendConfigureFlag previous.cardano-sl-networking "--ghc-option=-fno-warn-redundant-constraints";
    disciplina = (appendBuildFlag previous.disciplina "-Werror").overrideAttrs (super: { preConfigure = "${final.hpack}/bin/hpack ."; });
  };
}
