{ pkgs ? import ./closure.nix }: with pkgs;

stdenv.mkDerivation {
  name = "disciplina";

  nativeBuildInputs = [
    haskellPackages.tw
    hlint
  ];

  buildInputs = [
    binutils
    git
    gmp
    openssl
    rocksdb
    zeromq
    zlib
  ];
}
