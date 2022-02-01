FROM lnl7/nix:2.3.7

RUN nix-env -iA \
 nixpkgs.curl \
 nixpkgs.jq
