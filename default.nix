{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let 
  stack4nix = fetchGit {
    url = "https://github.com/serokell/stack4nix";
    rev = "26a46991270749ecc57d5842d391435b314dc26f";
  };

  buildStackProject = import stack4nix { inherit pkgs; };
  disciplinaPackages = buildStackProject (lib.cleanSource ./.);
in

disciplinaPackages // rec {
  disciplina-config = runCommand "config.yaml" {} "mkdir -p $out/etc/disciplina && cp ${./config.yaml} $_";
  disciplina-static = symlinkJoin {
    name = "disciplina-static";
    paths = [ disciplina-config ] ++ map haskell.lib.justStaticExecutables
      (with disciplinaPackages; [ disciplina-faucet disciplina-witness disciplina-educator ]);
  };
}
