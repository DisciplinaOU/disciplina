{ pkgs ? import ./closure.nix }: with pkgs;

let
  project = import ./. { inherit pkgs; };

  writeShellScript = source: writeTextFile {
    name = "script";
    executable = true;
    checkPhase = "${shellcheck}/bin/shellcheck $out";
    text = ''
      #!/bin/sh -e
      ${source}
    '';
  };
in

rec {
  disciplina-haddock = with lib;
    let
      drvs = filterAttrs (const (hasAttr "doc")) project;
      docs = mapAttrs (const (getOutput "doc")) drvs;
      globs = map (doc: "${doc}/share/doc/*") (attrValues docs);
    in
    runCommand "disciplina-haddock" {} ''
      mkdir $out

      for drv in ${concatStringsSep " " globs}; do
        ln -s $drv/html $out/$(basename $drv)
      done
    '';

  disciplina-wallet-macos-sandbox = writeShellScript ''
    sandbox-exec -D HOME="$HOME" -D DYLD_ROOT_PATH="$DYLD_ROOT_PATH" \
      -f ${./wallet/profile.sb} ${disciplina-wallet-wrapped}/bin/disciplina-wallet
  '';

  disciplina-wallet = haskell.lib.justStaticExecutables project.disciplina-wallet;

  disciplina-wallet-wrapped = writeShellScript ''
    ${disciplina-wallet}/bin/dscp-wallet --witness https://witness.disciplina.io \
      --config ${./config.yaml} --config-key alpha
  '';

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
}
