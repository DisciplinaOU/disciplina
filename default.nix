with import (fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
  config.allowUnfree = true;
};
 stdenv.mkDerivation {
  name = "srk-buildkite-scripts";
  src = ./.;
  buildPhase = "true";
  installPhase = ''
    install -Dm755 bin/* -t "$out/bin"
  '';
}
