{ pkgs ? import ./closure.nix }: with pkgs;

stdenv.mkDerivation rec {
  name = "disciplina";

  nativeBuildInputs = [
    binutils
    cachix
    git
    haskell.compiler.ghc822
    haskellPackages.tw
    hlint
    nix
    pkgconfig
    stack
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
