#! /usr/bin/env bash
# Requires bash >= 4.4 (empty-array expansion under set -u).
set -euo pipefail

SUITE="pure-noise-bench"
BFILE="bench/results/$(date -u -Iminutes)"
BASELINE="bench/results/current"
RUN_PRE=()
CABAL_FLAGS=(--flags=+llvm-bench)

if [ "${1:-}" = "fnl" ]; then
  SUITE="pure-noise-fnl-bench"
  BFILE="$BFILE-fnl"
  BASELINE="$BASELINE-fnl"
  RUN_PRE=(taskset -c 4)
  shift
fi

ARGS="--csv=$BFILE.csv"

case "${1:-tasty}" in
criterion)
  echo >&2 "using criterion"
  ARGS="$ARGS --output=$BFILE.html"
  ;;
tasty)
  echo >&2 "using tasty-bench"
  COMP=""
  if [ -f "$BASELINE.csv" ]; then
    echo "comparing with $BASELINE.csv"
    COMP="--baseline $BASELINE.csv"
  else
    echo "no baseline $BASELINE.csv from $PWD"
  fi
  ARGS="$ARGS --svg=$BFILE.svg --timeout 100 --stdev 1 --time-mode wall $COMP"
  ;;
*)
  echo >&2 "Unrecognized option: $1"
  exit 2
  ;;
esac

# --- environment sanity warnings (provenance recorded in $BFILE.meta) ---
for ps in /sys/class/power_supply/*/online; do
  if [ -e "$ps" ] && [ "$(cat "$ps")" != "1" ]; then
    echo >&2 "WARNING: $(basename "$(dirname "$ps")") offline — on battery? Results will be invalid."
  fi
done
GOV="$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor 2>/dev/null || echo unknown)"
[ "$GOV" = performance ] || echo >&2 "WARNING: governor is '$GOV', not 'performance'."

sysread() { cat "$1" 2>/dev/null || echo n/a; }

write_meta() {
  {
    echo "date-utc: $(date -u -Iseconds)"
    echo "uname: $(uname -a)"
    echo "cpu-model: $(grep -m1 'model name' /proc/cpuinfo | cut -d: -f2- | sed 's/^ *//')"
    echo "governor-cpu0: $(sysread /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)"
    echo "governor-cpu4: $(sysread /sys/devices/system/cpu/cpu4/cpufreq/scaling_governor)"
    echo "intel-no-turbo: $(sysread /sys/devices/system/cpu/intel_pstate/no_turbo)"
    for ps in /sys/class/power_supply/*/online; do
      [ -e "$ps" ] && echo "ac-online-$(basename "$(dirname "$ps")"): $(cat "$ps")"
    done
    echo "run-pre: ${RUN_PRE[*]:-<none>}"
    echo "cabal-flags: ${CABAL_FLAGS[*]}"
    echo "bench-args: $ARGS"
    echo "ghc: $(ghc --version 2>/dev/null || echo unknown)"
    echo "g++: $(g++ --version 2>/dev/null | head -n1 || echo unknown)"
    echo "opt: $(opt --version 2>/dev/null | grep -m1 -i version | sed 's/^ *//' || echo unknown)"
    echo "cabal: $(cabal --version 2>/dev/null | head -n1 || echo unknown)"
  } >"$BFILE.meta"
  echo >&2 "wrote $BFILE.meta"
}

# Written before the run so aborted runs still leave provenance.
write_meta

BUILD=(cabal build "$SUITE" "${CABAL_FLAGS[@]}")
echo >&2 "${BUILD[@]}"
"${BUILD[@]}"

# Same flag context as the build — never resolve a stale, differently-configured binary.
EXECUTABLE="$(cabal list-bin "$SUITE" "${CABAL_FLAGS[@]}")"
echo >&2 "$SUITE executable: $EXECUTABLE"

# shellcheck disable=SC2206 # ARGS is a deliberately word-split option string
RUN=(cabal exec -- "${RUN_PRE[@]}" "$EXECUTABLE" $ARGS)
echo >&2 "${RUN[@]}"
"${RUN[@]}"

echo "status: completed $(date -u -Iseconds)" >>"$BFILE.meta"
