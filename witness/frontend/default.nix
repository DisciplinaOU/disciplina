{ stdenv, witnessUrl ? "", yarn, parallel, brotli }:
stdenv.mkDerivation {
  name = "disciplina-witness-frontend";
  src = stdenv.lib.cleanSource ./.;
  WITNESS_API_URL = witnessUrl;
  HOME = ".";

  buildInputs = [ yarn parallel brotli ];
  buildPhase = ''
    yarn install
    yarn build
    find dist/ -type f \
      -not -name '*.jpg' \
      -not -name '*.png' \
      -not -name '*.webp' \
      -not -name '*.woff' \
      -not -name '*.woff2' | parallel brotli
  '';

  installPhase = ''
    mv dist $out
  '';
}
