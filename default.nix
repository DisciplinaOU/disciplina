{ pkgs ? import ./closure.nix, shell ? false }: with pkgs;

let
  filterWhiteBlack = { path, whitelist, blacklist ? [] }: name: type:
    let
      relPath = (lib.removePrefix (toString path + "/") name) + (if type == "directory" then "/" else "");
      reMatch = (re: builtins.match re relPath != null);
    in
    (lib.any reMatch whitelist) && !(lib.any reMatch blacklist);

  composeFilters = a: b: name: type: (a name type) && (b name type);

  # TODO: fix this in stack-to-nix instead
  root = builtins.path rec {
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
in

stackToNix { inherit root shell; }
