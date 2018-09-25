{ pkgs ? import ./closure.nix }: with pkgs;

stdenv.mkDerivation rec {
  name = "disciplina";

  nativeBuildInputs = [
    binutils
    haskell.compiler.ghc822
    pkgconfig
  ];

  buildInputs = [
    gmp
    openssl
    rocksdb
    zeromq
    zlib
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
  '';
}