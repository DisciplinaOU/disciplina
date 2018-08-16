{ pkgs ? (import <nixpkgs> {}), stdenv ? pkgs.stdenv }:
with pkgs;
stdenv.mkDerivation {
  name = "disciplina-faucet-frontend";
  src = lib.cleanSource ./.;
  buildInputs = [ pkgs.yarn pkgs.parallel pkgs.brotli ];
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
  installPhase = "cp -R dist/ $out";
}
