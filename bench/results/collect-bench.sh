#! /usr/bin/env bash
SUITE="pure-noise-bench"
BFILE="bench/results/$(date -u -Iminutes)"
BASELINE="bench/results/current"
RUN_PRE=()

if [ "$1" = "fnl" ]; then
  SUITE="pure-noise-fnl-bench"
  BFILE="$BFILE-fnl"
  BASELINE="$BASELINE-fnl"
  RUN_PRE=(taskset -c 4)
  [ $# -gt 0 ] && shift
fi

ARGS="--csv=$BFILE.csv"

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
  ARGS="$ARGS --svg=$BFILE.svg --timeout 100 --stdev 1 --time-mode wall $COMP"
else
  echo >&2 "Unrecognized option: $1"
fi
shift

BUILD=(cabal build "$SUITE")
echo >&2 "${BUILD[@]}"
"${BUILD[@]}"

EXECUTABLE="$(cabal list-bin "$SUITE")"
echo >&2 "$SUITE executable: $EXECUTABLE"

RUN=(cabal exec -- "${RUN_PRE[@]}" "$EXECUTABLE" $ARGS)
echo >&2 "${RUN[@]}"
"${RUN[@]}"
