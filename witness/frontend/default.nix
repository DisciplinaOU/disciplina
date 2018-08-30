{ stdenv, nodejs, yarn, witnessURL ? "" }:

stdenv.mkDerivation {
  name = "disciplina-witness-frontend";
  src = stdenv.lib.cleanSource ./.;

  buildInputs = [ nodejs yarn ];

  WITNESS_API_URL = witnessURL;
  HOME = ".";

  buildPhase = ''
    yarn install
    yarn run build
  '';

  installPhase = ''
    mv dist $out
  '';
}
