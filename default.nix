with import <nixpkgs> { overlays = [ (import <serokell-overlay/pkgs>) ]; };
with haskell.lib;

buildStackApplication {
  package = "disciplina";
  src = lib.cleanSource ./.;
  ghc = haskell.compiler.ghc822;

  overrides = final: previous: {
    rocksdb-haskell = dependCabal previous.rocksdb-haskell [ rocksdb ];
    cardano-sl-networking = appendConfigureFlag previous.cardano-sl-networking "--ghc-option=-fno-warn-redundant-constraints";
    disciplina = doCheck (overrideCabal (appendConfigureFlag previous.disciplina "--ghc-option=-Werror") (super: with final; {
      buildDepends = (super.buildDepends or []) ++ [ hspec tasty tasty-discover tasty-hspec ];
      preConfigure = "${hpack}/bin/hpack .";
    }));
  };
}
