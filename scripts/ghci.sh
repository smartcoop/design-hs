#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc --interactive \
  -iexe/bin/ \
  -ilib/src/ \
  -hide-package base \
  -XDerivingVia \
  -XRecordWildCards \
  -XFlexibleInstances \
  -XFlexibleContexts \
  -XLambdaCase \
  -XTypeApplications \
  -XScopedTypeVariables \
  -XGADTs \
  -XOverloadedStrings \
  exe/bin/Main.hs
