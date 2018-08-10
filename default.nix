with import (fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
  config.allowUnfree = true;
};
 stdenv.mkDerivation {
  name = "srk-buildkite-scripts";
  src = ./.;
  buildPhase = "true";
  buildInputs = [ makeWrapper curl ];
  installPhase = ''
    install -Dm755 bin/* -t "$out/bin"
    wrapProgram $out/bin/deploy-result.sh \
      --suffix PATH : ${curl}/bin
  '';
}
