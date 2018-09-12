{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let
  buildFlatpak = callPackage (fetchGit {
    url = "https://github.com/serokell/nix-flatpak";
    rev = "3ceb79f92e80c84a4360badd721ff87a214a6932";
  }) {};

  buildMacOSApp = callPackage (fetchGit {
    url = "https://github.com/serokell/nix-macos-app";
    rev = "192f3c22b4270be84aef9176fdf52a41d0d85b32";
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

  disciplina-wallet-macos-sandbox = writeShellScript ''
    sandbox-exec -D HOME="$HOME" -D DYLD_ROOT_PATH="$DYLD_ROOT_PATH" \
      -f ${./wallet/profile.sb} ${disciplina-wallet-wrapped}/bin/disciplina-wallet
  '';

  disciplina-wallet = haskell.lib.justStaticExecutables project.disciplina-wallet;

  disciplina-wallet-wrapped = writeShellScript ''
    ${disciplina-wallet}/bin/dscp-wallet --witness https://witness.disciplina.io
  '';

  self = {
    disciplina-wallet-flatpak = buildFlatpak {
      app-id = "io.disciplina.Wallet";
      command = disciplina-wallet-wrapped;
      finish-args = [ "--share=network" ];
    };

    disciplina-wallet-macos-app = buildMacOSApp {
      name = "Disciplina";
      target = disciplina-wallet-macos-sandbox;
      withOpen = true;
    };
  };
in

with lib;

let
  systemSet = systems.elaborate { inherit system; };
in

filterAttrs (_: drv: any (meta.platformMatch systemSet) drv.meta.platforms) self
