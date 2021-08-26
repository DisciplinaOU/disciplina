{ pkgs ? import ./pkgs.nix }: with pkgs;

let
  project = import ./. { inherit pkgs; };
  source = constGitIgnore "disciplina-release-src" ./. [ "*.icns" "*.png" ];

  runCheck = source: runCommand "check" {} ''
    ${source}
    touch $out
  '';

  writeShellScript = source: writeTextFile {
    name = "script";
    executable = true;
    checkPhase = "${shellcheck}/bin/shellcheck $out";
    text = ''
      #!${stdenv.shell} -e
      ${source}
    '';
  };

  justDataOutputs = drv: lib.optional (drv ? data) drv.data;

  server-packages = with project; [
    disciplina-educator
    disciplina-multi-educator
    disciplina-faucet
    disciplina-tools
    disciplina-witness
    disciplina-pdfs
  ];
in

rec {
  disciplina = symlinkJoin {
    name = "disciplina";
    paths = map haskell.lib.justStaticExecutables server-packages;
  };

  disciplina-data = symlinkJoin {
    name = "disciplina-data";
    paths = lib.concatMap justDataOutputs server-packages;
  };

  inherit (pkgs) pdf-generator-xelatex;
  inherit (project) disciplina-tools;

  disciplina-config = runCommand "disciplina-config.yaml" {} "cp ${./config.yaml} $out";

  disciplina-haddock = with lib;
    let
      docs = remove isNull (map (drv: drv.doc or null) (attrValues project));
      globs = map (doc: "${doc}/share/doc/*") docs;
    in
    runCommand "disciplina-haddock.tar.gz" {} ''
      for drv in ${concatStringsSep " " globs}; do
        ln -s $drv/html $(basename $drv)
      done

      tar czfh $out *
    '';

  disciplina-hlint = runCommand "hlint.html" {} ''
    ${hlint}/bin/hlint ${source} --no-exit-code --report=$out -j
  '';

  disciplina-trailing-whitespace = runCheck ''
    for f in $(find ${source} -type f -not -name "*.jpg" -not -name "*.png" -not -name "*.otf"); do
      ${haskellPackages.tw}/bin/tw $f
    done
  '';

  disciplina-wallet-macos-sandbox = writeShellScript ''
    sandbox-exec -D HOME="$HOME" -D DYLD_ROOT_PATH="$DYLD_ROOT_PATH" \
      -f ${./wallet/profile.sb} ${disciplina-wallet-wrapped}/bin/disciplina-wallet
  '';

  disciplina-wallet = haskell.lib.justStaticExecutables project.disciplina-wallet;

  disciplina-wallet-flatpak = buildFlatpak {
    app-id = "io.disciplina.Wallet";
    command = disciplina-wallet-flatpak-wrapper;
    finish-args = [ "--share=network" ];
  };

  disciplina-wallet-flatpak-wrapper = writeShellScript ''
    ${disciplina-wallet}/bin/dscp-wallet --witness https://witness.disciplina.io \
      --config ${./config.yaml} --config-key alpha
  '';

  disciplina-wallet-macos-app = buildMacOSApp {
    name = "Disciplina";
    icon = ./wallet/icon.icns;
    target = disciplina-wallet-macos-wrapper;
    withOpen = true;
  };

  disciplina-wallet-macos-wrapper = writeShellScript ''
    export TERMINFO=/usr/share/terminfo

    ${disciplina-wallet}/bin/dscp-wallet --witness https://witness.disciplina.io \
      --config ${./config.yaml} --config-key alpha
  '';

  disciplina-wallet-macos-zip = runCommand "disciplina-wallet-macos.zip" {} ''
    cd ${disciplina-wallet-macos-app}/Applications
    ${zip}/bin/zip -r --symlinks $out Disciplina.app
  '';
}
