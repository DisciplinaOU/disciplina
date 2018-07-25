with import <nixpkgs> { overlays = [ (import <serokell-overlay/pkgs>) ]; };
with haskell.lib;

buildStackApplication {
  package = "disciplina-educator";

  # Running hpack manually before the build is required
  # because of the problem in stack2nix -- it builds every
  # subpackage in a separate environment, thus moving
  # hpack directory out of the scope.
  #
  # See: https://github.com/input-output-hk/stack2nix/pull/107

  src = runCommand "source" { cwd = lib.cleanSource ./.; } ''
    cp --no-preserve=mode,ownership -r $cwd $out

    for f in $out/{core,witness,educator,tools,wallet}; do
      ${haskellPackages.hpack}/bin/hpack $f
    done
  '';

  ghc = haskell.compiler.ghc822;

  overrides = 
    final: previous: 
    let overridingSet = (super: with final; {
          configureFlags = [ "--ghc-option=-Werror" ];
          doCheck = true;
          testDepends = [ hspec tasty tasty-discover tasty-hspec ];
        }); 
        overrideModule = prev: overrideCabal prev (overridingSet final);
    in {
      rocksdb-haskell = dependCabal previous.rocksdb-haskell [ rocksdb ];

      # Is it possible to minimise this boilerplate?
      disciplina-core = overrideModule previous.disciplina-core;
      disciplina-witness = overrideModule previous.disciplina-witness;
      disciplina-educator = overrideModule previous.disciplina-educator;
      disciplina-wallet = overrideModule previous.disciplina-wallet;
      disciplina-tools = overrideModule previous.disciplina-tools;
    };
}
