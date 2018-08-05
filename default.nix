with import <nixpkgs> { overlays = [ (import <serokell-overlay/pkgs>) ]; };
with lib;
with haskell.lib;
let
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

  packages = [
    "disciplina-core" "disciplina-witness" "disciplina-educator"
    "disciplina-wallet" "disciplina-tools"
  ];

  getAttrs = attrs: set: genAttrs attrs (name: set.${name});

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
    } // (mapAttrs (const overrideModule) (getAttrs packages previous));

  closure = stackClosure ghc src overrides;

in
  getAttrs packages closure

