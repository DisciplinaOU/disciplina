let
  nixpkgs = fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz";
  serokell-overlay = fetchGit "ssh://git@github.com:/serokell/serokell-overlay";
in

import nixpkgs { overlays = [ (import "${serokell-overlay}/pkgs") ]; }
