#! /usr/bin/env bash

sigint_handler()
{
  kill $(jobs -p)
  exit
}

trap sigint_handler SIGINT

TARGET_DIR=exe
TARGET=design-hs-exe
CMD=$1
BUILD_DIR=dist-$TARGET-$CMD
ADDITIONAL_WATCH=""

HTML_OUTDIR=/home/ashesh/tmp/design-hs/
HTML_EXAMPLES_OUTDIR=examples

while true; do
  # $@ &
    case $CMD in
	haddock) cabal haddock &
		 ;;
	ghcid) ghcid -l -c "cabal new-repl $TARGET --builddir $BUILD_DIR" &
	       ;;
	run) cabal run $TARGET \
		   --disable-optimisation \
		   --builddir $BUILD_DIR -- \
		   -D $HTML_OUTDIR \
		   -E $HTML_EXAMPLES_OUTDIR \
	     &
	     ADDITIONAL_WATCH="-r lib"
	     ;;
	*) echo "Unknown cmd ('$CMD'), must be either 'run','ghcid' or 'haddock'"
	   break;
	   ;;
    esac

  # run_server &
  PID1=$!

  inotifywait \
      -e modify \
      -e move \
      -e create \
      -e delete \
      -e attrib \
      $ADDITIONAL_WATCH \
      -r $TARGET_DIR \
      $TARGET_DIR/$TARGET.cabal \
      --exclude ".*flycheck.*|.*\#.*"
  kill $(jobs -p)
  sleep 3
done
