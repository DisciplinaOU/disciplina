with import (fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
  config.allowUnfree = true;
  overlays = [ (import "${fetchGit "ssh://git@github.com:/serokell/serokell-overlay"}/pkgs") ];
};

with haskell.lib;

let
  getAttrs = attrs: set: lib.genAttrs attrs (name: set.${name});
  dscp-packages = buildStackApplication rec {
  packages = [
    "disciplina-core" "disciplina-witness" "disciplina-educator"
    "disciplina-wallet" "disciplina-tools" "disciplina-faucet"
  ];

  # Running hpack manually before the build is required
  # because of the problem in stack2nix -- it builds every
  # subpackage in a separate environment, thus moving
  # hpack directory out of the scope.
  #
  # See: https://github.com/input-output-hk/stack2nix/pull/107

  src = runCommand "source" { cwd = lib.cleanSource ./.; } ''
    cp --no-preserve=mode,ownership -r $cwd $out

    for f in $out/{core,witness,educator,tools,wallet,faucet}; do
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
          postInstall = ''
            mkdir -p $out/share/disciplina/specs $out/etc/disciplina
            cp ${src}/configuration.yaml $_
            find ${src}/specs -name '*.yaml' -exec cp '{}' $out/share/disciplina/specs \;
          '';
        });
        overrideModule = prev: overrideCabal prev (overridingSet final);
    in {
      rocksdb-haskell = dependCabal previous.rocksdb-haskell [ rocksdb ];
    } // (lib.mapAttrs (lib.const overrideModule) (getAttrs packages previous));
  };
in
  dscp-packages // {
  disciplina-faucet-frontend = pkgs.callPackage ./faucet/frontend {};
  disciplina-witness-frontend = pkgs.callPackage ./witness/frontend {};
  disciplina-wallet = haskell.lib.justStaticExecutables dscp-packages.disciplina-wallet;
  disciplina-bin = pkgs.runCommandNoCC "disciplina-bin-${dscp-packages.disciplina-core.version}" {} ''
    mkdir $out
    ${pkgs.rsync}/bin/rsync -Labu --no-perms --exclude lib/ --exclude propagated-build-inputs --inplace \
      ${lib.concatMapStringsSep " " (f: "${f}/") (builtins.attrValues dscp-packages)} $out/
  '';
}
