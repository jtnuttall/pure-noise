#! /usr/bin/env bash
BFILE="bench/results/$(date -u -Iminutes)"
BASELINE="bench/results/current"
STACKOPTS="--force-dirty"
ARGS="--csv=$BFILE.csv +RTS -T -RTS"

if [ "$1" = "llvm" ]; then
  STACKOPTS="$STACKOPTS --ghc-options=-fllvm"
  BFILE="$BFILE-llvm"
  BASELINE="$BASELINE-llvm"
  shift
fi

if [ "$1" = "criterion" ]; then
  echo >&2 "using criterion"
  ARGS="$ARGS --output=$BFILE.html"
elif [ -z "$1" ] || [ "$1" = "tasty" ]; then
  echo >&2 "using tasty-bench"
  COMP=""
  if [ -f "$BASELINE.csv" ]; then
    echo "comparing with $BASELINE.csv"
    COMP="--baseline $BASELINE.csv"
  else
    echo "no baseline $BASELINE.csv from $PWD"
  fi
  ARGS="$ARGS --svg=$BFILE.svg --timeout 100 --stdev 1 $COMP"
else
  echo >&2 "Unrecognized option: $1"
fi
shift

echo >&2 stack bench $STACKOPTS --ba="$ARGS"
stack bench $STACKOPTS --ba="$ARGS"
