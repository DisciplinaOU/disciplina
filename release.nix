{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let
  buildFlatpak = callPackage (fetchGit {
    url = "https://github.com/serokell/nix-flatpak";
    rev = "46a2aadf37981d6313621913cd2802debfb763fd";
  }) {};

  buildMacOSApp = callPackage (fetchGit {
    url = "https://github.com/serokell/nix-macos-app";
    rev = "bac301ef897b55fac97c63cc0d1dbc3c492065e8";
  }) {};
  
  project = import ./. { inherit pkgs; };
in

rec {
  disciplina-wallet = haskell.lib.justStaticExecutables project.disciplina-wallet;

  disciplina-wallet-wrapped = writeShellScriptBin "disciplina-wallet" ''
    ${disciplina-wallet}/bin/dscp-wallet --witness https://witness.disciplina.io
  '';

  disciplina-wallet-flatpak = buildFlatpak {
    app-id = "io.disciplina.Wallet";
    command = "${disciplina-wallet-wrapped}/bin/disciplina-wallet";
    finish-args = [ "--share=network" ];
  };

  disciplina-wallet-macos-app = buildMacOSApp {
    name = "Disciplina";
    target = "${disciplina-wallet-wrapped}/bin/disciplina-wallet";
    withOpen = true;
  };
}
