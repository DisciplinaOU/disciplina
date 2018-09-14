{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let 
  stack4nix = fetchGit {
    url = "https://github.com/serokell/stack4nix";
    rev = "26a46991270749ecc57d5842d391435b314dc26f";
  };

  filterWhiteBlack = { path, whitelist, blacklist ? [] }: name: type: let
    relPath = (lib.removePrefix (toString path + "/") name) + (if type == "directory" then "/" else "");
    reMatch = (re: builtins.match re relPath != null);
  in
    (lib.any reMatch whitelist) && !(lib.any reMatch blacklist);
  composeFilters = a: b: name: type: (a name type) && (b name type);

  buildStackProject = import stack4nix { inherit pkgs; };
  disciplinaPackages = buildStackProject (builtins.path rec {
    path = ./.;
    name = "disciplina";
    filter = composeFilters (filterWhiteBlack {
      inherit path;
      whitelist = [ "stack\.yaml" ".*/.*" ];
      blacklist = [ ".*node_modules.*" "docs.*" "run.*" "scripts.*" "secrets.*" "specs.*" ];
    }) (lib.cleanSourceFilter);
  });
in

disciplinaPackages // rec {
  disciplina-config = runCommand "config.yaml" {} "mkdir -p $out/etc/disciplina && cp ${./config.yaml} $_/config.yaml";
  disciplina-static = symlinkJoin {
    name = "disciplina-static";
    paths = [ disciplina-config ] ++ map haskell.lib.justStaticExecutables
      (with disciplinaPackages; [ disciplina-faucet disciplina-witness disciplina-educator ]);
  };
}
