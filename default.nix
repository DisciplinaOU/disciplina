{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let 
  stack4nix = fetchGit {
    url = "https://github.com/serokell/stack4nix";
    rev = "26a46991270749ecc57d5842d391435b314dc26f";
  };

  selectAttrs = names: set:
    lib.genAttrs names (name: lib.getAttr name set);

  packages = [
    "disciplina-core" "disciplina-witness" "disciplina-educator"
    "disciplina-wallet" "disciplina-tools" "disciplina-faucet"
  ];

  src = lib.cleanSource ./.;

  overrides = with haskell.lib; final: previous:
    let
      defaults = super: with final; {
        configureFlags = [ "--ghc-option=-Werror" ];

        buildTools = (super.buildTools or []) ++ [ autoexporter tasty-discover ];
        testDepends = (super.testDepends or []) ++ [ hspec tasty tasty-discover tasty-hspec ];

        doCheck = true;
        doHaddock = false; # until syntax errors in comments are fixed

        postInstall = ''
          mkdir -p $out/share/disciplina/specs $out/etc/disciplina
          cp ${src}/configuration.yaml $_
          find ${src}/specs -name '*.yaml' -exec cp '{}' $out/share/disciplina/specs \;
        '';
      };

      overrideDefaults = attr: overrideCabal attr (defaults final);
    in
    lib.mapAttrs (lib.const overrideDefaults) (selectAttrs packages previous);

  buildStackProject = import stack4nix { inherit pkgs overrides; };
in

buildStackProject src
