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

  writeShellScript = source: writeTextFile {
    name = "script";
    executable = true;
    checkPhase = "${shellcheck}/bin/shellcheck $out";
    text = ''
      #!/bin/sh
      ${source}
    '';
  };
in

rec {
  disciplina-wallet = haskell.lib.justStaticExecutables project.disciplina-wallet;

  disciplina-wallet-wrapped = writeShellScript ''
    ${disciplina-wallet}/bin/dscp-wallet --witness https://witness.disciplina.io
  '';

  disciplina-wallet-flatpak = buildFlatpak {
    app-id = "io.disciplina.Wallet";
    command = disciplina-wallet-wrapped;
    finish-args = [ "--share=network" ];
  };

  disciplina-wallet-macos-sandbox = writeShellScript ''
    sandbox-exec -D HOME="$HOME" -D DYLD_ROOT_PATH="$DYLD_ROOT_PATH" \
      -f ${./wallet/profile.sb} ${disciplina-wallet-wrapped}/bin/disciplina-wallet
  '';

  disciplina-wallet-macos-app = buildMacOSApp {
    name = "Disciplina";
    target = disciplina-wallet-macos-sandbox;
    withOpen = true;
  };
}
