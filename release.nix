{ pkgs ? import ./pkgs.nix }: with pkgs;

let
  project = import ./. { inherit pkgs; };
  source = constGitIgnore "disciplina-release-src" ./. [ "*.icns" "*.png" ];

  justDataOutputs = drv: lib.optional (drv ? data) drv.data;

  fetchShare = { name, path }: runCommand name { inherit path; }
    ''
    ln -s $path/share $out
    '';

  wrapInDir = name: path: runCommand name { inherit name path; }
    ''
    mkdir -p $out
    ln -s $path $out/$name
    '';

  # server-packages = with project; [
  #   disciplina-educator
  #   disciplina-multi-educator
  #   disciplina-pdfs
  # ];

in

rec {
  disciplina = symlinkJoin {
    name = "disciplina";
    # paths = map haskell.lib.justStaticExecutables server-packages;
    paths = [
      project.disciplina-educator.components.exes.dscp-educator
      project.disciplina-multi-educator.components.exes.dscp-multi-educator
    ];
  };

  disciplina-data = fetchShare {
    name = "disciplina-data";
    path = project.disciplina-pdfs.components.exes.dscp-inject-json-into-pdf;
  };


  inherit (pkgs) pdf-generator-xelatex;
  # inherit (project) disciplina-tools;

  disciplina-config = runCommand "disciplina-config.yaml" {} "cp ${./config.yaml} $out";

  disciplina-with-config = symlinkJoin {
    name = "disciplina-with-cfg";
    paths = [
      disciplina
      (wrapInDir "config.yaml" disciplina-config)
    ];
  };

  disciplina-multieducator-docker = dockerTools.buildImage {
    name = "disciplina-multi-educator";
    tag = "latest";
    contents = [
      bash
      coreutils
      cacert
      disciplina-with-config
    ];

    fromImage = dockerTools.pullImage {
      imageName = "postgres";
      imageDigest = "sha256:d3b857fac70936a2e0dd83226cc113db1787a4f6119df77a798d8137354c4989";
      sha256 = "061w4sagqrh2d9jiyfc1nr7x912l477wvvb2r78n4ns4fzasli5h";
      finalImageTag = "14.2-alpine";
      finalImageName = "postgres";
    };

    config = {
      Cmd = [
        "/bin/dscp-multi-educator"
        "--config" "/config.yaml"
        "--config-key" "new-web3"
      ];
      Env = [
        "LANG=\"C.UTF-8\""
        "LC_ALL=\"C.UTF-8\""
      ];
      ExposedPorts = {
        "4040/tcp" = {};
      };
    };
  };

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

  # disciplina-wallet-macos-sandbox = writeShellScript ''
  #   sandbox-exec -D HOME="$HOME" -D DYLD_ROOT_PATH="$DYLD_ROOT_PATH" \
  #     -f ${./wallet/profile.sb} ${disciplina-wallet-wrapped}/bin/disciplina-wallet
  # '';

  # disciplina-wallet = haskell.lib.justStaticExecutables project.disciplina-wallet;

  # disciplina-wallet-flatpak = buildFlatpak {
  #   app-id = "io.disciplina.Wallet";
  #   command = disciplina-wallet-flatpak-wrapper;
  #   finish-args = [ "--share=network" ];
  # };

  # disciplina-wallet-flatpak-wrapper = writeShellScript ''
  #   ${disciplina-wallet}/bin/dscp-wallet --witness https://witness.disciplina.io \
  #     --config ${./config.yaml} --config-key alpha
  # '';

  # disciplina-wallet-macos-app = buildMacOSApp {
  #   name = "Disciplina";
  #   icon = ./wallet/icon.icns;
  #   target = disciplina-wallet-macos-wrapper;
  #   withOpen = true;
  # };

  # disciplina-wallet-macos-wrapper = writeShellScript ''
  #   export TERMINFO=/usr/share/terminfo

  #   ${disciplina-wallet}/bin/dscp-wallet --witness https://witness.disciplina.io \
  #     --config ${./config.yaml} --config-key alpha
  # '';

  # disciplina-wallet-macos-zip = runCommand "disciplina-wallet-macos.zip" {} ''
  #   cd ${disciplina-wallet-macos-app}/Applications
  #   ${zip}/bin/zip -r --symlinks $out Disciplina.app
  # '';
}
