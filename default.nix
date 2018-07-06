with import <nixpkgs> { overlays = [ (import <serokell-overlay/pkgs>) ]; };
with haskell.lib;

buildStackApplication {
  package = "disciplina";
  src = lib.cleanSource ./.;
  ghc = haskell.compiler.ghc822;

  overrides = final: previous: {
    rocksdb-haskell = dependCabal previous.rocksdb-haskell [ rocksdb ];
    disciplina = overrideCabal previous.disciplina (super: with final; {
      configureFlags = [ "--ghc-option=-Werror" ];
      doCheck = true;
      preConfigure = "${hpack}/bin/hpack .";
      testDepends = [ hspec tasty tasty-discover tasty-hspec ];
    });
  };
}
