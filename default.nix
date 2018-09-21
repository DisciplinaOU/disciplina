{ pkgs ? import ./closure.nix }: with pkgs;

let
  filterWhiteBlack = { path, whitelist, blacklist ? [] }: name: type:
    let
      relPath = (lib.removePrefix (toString path + "/") name) + (if type == "directory" then "/" else "");
      reMatch = (re: builtins.match re relPath != null);
    in
    (lib.any reMatch whitelist) && !(lib.any reMatch blacklist);

  composeFilters = a: b: name: type: (a name type) && (b name type);

  # TODO: fix this in stack-to-nix instead
  src = builtins.path rec {
    path = ./.;
    name = "disciplina";
    filter = composeFilters (filterWhiteBlack {
      inherit path;
      whitelist = [
        "stack\\.yaml"
        ".*"
      ];
      blacklist = [
        "\\.buildkite/.*"
        "docs/.*"
        "run/.*"
        "scripts/.*"
        "secrets/.*"
        "specs/.*"
        ".*\\.nix"
      ];
    }) lib.cleanSourceFilter;
  };

  disciplinaPackages = stackToNix { inherit src; };
in

disciplinaPackages // rec {
  disciplina-config = runCommand "config.yaml" {} "mkdir -p $out/etc/disciplina && cp ${./config.yaml} $_/config.yaml";
  disciplina-static = symlinkJoin {
    name = "disciplina-static";
    paths = [ disciplina-config ] ++ map haskell.lib.justStaticExecutables
      (with disciplinaPackages; [ disciplina-faucet disciplina-witness disciplina-educator ]);
  };
}
