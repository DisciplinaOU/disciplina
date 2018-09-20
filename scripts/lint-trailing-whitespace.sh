#! /usr/bin/env nix-shell
#! nix-shell -i sh ../shell.nix

set -e

for f in $(find . \( -name \*.hs -or -name \*.nix -or -name \*.sh -or -name \*.yaml \)); do
  tw $f
done
