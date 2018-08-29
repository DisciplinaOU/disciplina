{ stenv, nodejs, yarn }:

stdenv.mkDerivation {
  name = "disciplina-frontend";
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
