#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

find exe/ lib/ src/ -name '*.hs' \
  -print \
  -exec brittany --write-mode inplace {} \;
