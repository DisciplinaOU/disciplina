with import <nixpkgs> {
  config = {
    packageOverrides = pkgs: {
      jemalloc = pkgs.jemalloc.overrideAttrs (oldAttrs: rec {
        name = "jemalloc-${version}";
        version = "4.5.0";
        src = pkgs.fetchurl {
          url = "https://github.com/jemalloc/jemalloc/releases/download/${version}/${name}.tar.bz2";
          sha256 = "9409d85664b4f135b77518b0b118c549009dc10f6cba14557d170476611f6780";
        };
      });
    };
  };
};

let
  hsPkgs = haskell.packages.ghc822;
  environment.variables.TASTY_QUICKCHECK_SHOW_REPLAY=true
in
  haskell.lib.buildStackProject {
    name = "disciplina";
    ghc = hsPkgs.ghc;
    buildInputs = [
      openssl
      zlib
      zeromq
      binutils
      git
      rocksdb
      gmp
      hsPkgs.cpphs
      hsPkgs.happy
    ];
  }
