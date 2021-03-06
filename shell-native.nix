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
    pdf-generator-xelatex
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
  '';
}
