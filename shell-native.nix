{ pkgs ? import ./pkgs.nix }: with pkgs;

stdenv.mkDerivation rec {
  name = "disciplina";

  nativeBuildInputs = [
    binutils
    haskell.compiler.ghc822
    pkgconfig
    postgresql
    git
  ];

  buildInputs = [
    git
    gmp
    openssl
    rocksdb
    zeromq
    zlib
    (texlive.combine {
      inherit (texlive)
        collection-basic
        collection-fontsrecommended
        collection-langcyrillic
        collection-xetex
        scheme-basic
        extsizes
        titlesec
        url
        hyperref
        xltxtra
        geometry
        background
        realscripts
        datetime2
        tracklang
        etoolbox
        everypage
        xkeyval
        xcolor
        pgf
        fontspec;
    })
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
  '';
}
