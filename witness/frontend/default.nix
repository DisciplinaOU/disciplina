{ stdenv, nodejs, yarn }:

stdenv.mkDerivation {
  name = "disciplina-witness-frontend";
  src = stdenv.lib.cleanSource ./.;

  buildInputs = [ nodejs yarn ];

  HOME = ".";

  buildPhase = ''
    yarn install
    yarn run build
  '';

  installPhase = ''
    mv dist $out
  '';
}
