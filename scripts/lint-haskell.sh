#! /usr/bin/env nix-shell
#! nix-shell -i sh ../shell.nix

hlint . --no-exit-code --report=hlint.html -j
