with import <nixpkgs> {
  overlays = [
    (import ''${builtins.fetchGit "ssh://git@github.com:/serokell/serokell-overlay.git"}/pkgs'')
  ];
};

with haskell.lib;

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
