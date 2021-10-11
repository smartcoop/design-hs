#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

mkdir -p _site
make
