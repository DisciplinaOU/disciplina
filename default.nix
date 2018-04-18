let
  nixpkgs = import "${overlay}/nixpkgs.nix";
  overlay = builtins.fetchGit {
    url = "ssh://git@github.com:/serokell/serokell-ops.git";
  };
in

with nixpkgs;

buildStack {
  package = "disciplina";
  src = lib.cleanSource ./.;
  ghc = pkgs.haskell.compiler.ghc822;

  overrides = final: previous: {
    rocksdb-haskell = dependCabal previous.rocksdb-haskell [ rocksdb ];
  };
}
