#!/bin/sh

set -u

usage() {
  cat <<'USAGE'
Usage: scripts/test.sh SUITE [--log-dir DIR] [--label LABEL]

Suites:
  quick   Run the complete @correctness gate.
  stress  Run randomized and concurrent @stress pressure.

Options:
  --log-dir DIR  Write reports under DIR instead of .logs.
  --label LABEL  Add LABEL to the report filename.
  -h, --help     Show this help.
USAGE
}

suite=
log_dir=.logs
label=

while [ "$#" -gt 0 ]; do
  case "$1" in
    quick | stress)
      if [ -n "$suite" ]; then
        echo "Only one SUITE may be provided." >&2
        exit 2
      fi
      suite=$1
      shift
      ;;
    --log-dir)
      if [ "$#" -lt 2 ]; then
        echo "--log-dir requires a value." >&2
        exit 2
      fi
      log_dir=$2
      shift 2
      ;;
    --label)
      if [ "$#" -lt 2 ]; then
        echo "--label requires a value." >&2
        exit 2
      fi
      label=$2
      shift 2
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [ -z "$suite" ]; then
  echo "Missing SUITE." >&2
  usage >&2
  exit 2
fi

if ! root=$(git rev-parse --show-toplevel 2>/dev/null); then
  echo "scripts/test.sh must run inside a Git checkout." >&2
  exit 2
fi

cd "$root" || exit 2
mkdir -p "$log_dir"

slug_source=${label:-$suite}
slug=$(printf "%s" "$slug_source" | tr -c "A-Za-z0-9_.-" "-" | sed 's/--*/-/g; s/^-//; s/-$//')
if [ -z "$slug" ]; then
  slug=$suite
fi

timestamp=$(date -u +%Y%m%dT%H%M%SZ)
report="$log_dir/observe-test-$slug-$timestamp.log"
latest="$log_dir/observe-test-$slug-latest.log"
artifact_dir="$log_dir/observe-test-$slug-$timestamp-artifacts"

append_line() {
  printf "%s\n" "$*" | tee -a "$report"
}

append_file() {
  cat "$1" | tee -a "$report"
}

capture_failure_outputs() {
  marker=$1
  output_list="$report.outputs.$$"

  find "$root/_build" -path "*/_build/_tests/*" -name "*.output" -type f \
    -newer "$marker" -print >"$output_list" 2>/dev/null || true

  if [ ! -s "$output_list" ]; then
    rm -f "$output_list"
    return
  fi

  mkdir -p "$artifact_dir"
  append_line ""
  append_line "Alcotest outputs copied to $artifact_dir:"
  index=0
  while IFS= read -r output; do
    if [ ! -f "$output" ]; then
      continue
    fi
    index=$((index + 1))
    artifact=$(printf "alcotest-%03d.output" "$index")
    cp "$output" "$artifact_dir/$artifact"
    append_line "- $output"
  done <"$output_list"
  rm -f "$output_list"
}

run_gate() {
  alias_name=$1
  temporary="$report.tmp.$$"
  marker="$report.marker.$$"
  : >"$marker"

  append_line ""
  append_line "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] START @$alias_name"
  append_line "$ opam exec -- dune build --root $root --force @$alias_name"

  opam exec -- dune build --root "$root" --force "@$alias_name" \
    >"$temporary" 2>&1
  status=$?
  append_file "$temporary"
  rm -f "$temporary"
  append_line "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] EXIT @$alias_name status=$status"

  if [ "$status" -ne 0 ]; then
    capture_failure_outputs "$marker"
  fi
  rm -f "$marker"
  return "$status"
}

{
  echo "# Observe test report"
  echo "started_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "suite=$suite"
  echo "root=$root"
  echo "branch=$(git branch --show-current 2>/dev/null || true)"
  echo "head=$(git rev-parse --short HEAD 2>/dev/null || true)"
  echo "OBSERVE_QCHECK_COUNT=${OBSERVE_QCHECK_COUNT:-}"
  echo "QCHECK_SEED=${QCHECK_SEED:-}"
  echo "OBSERVE_RACE_TRIALS=${OBSERVE_RACE_TRIALS:-}"
  echo "OBSERVE_CONCURRENCY_WORK=${OBSERVE_CONCURRENCY_WORK:-}"
  echo "dune=$(opam exec -- dune --version 2>/dev/null || true)"
  echo "ocaml=$(opam exec -- ocamlc -version 2>/dev/null || true)"
  echo "system=$(uname -a 2>/dev/null || true)"
  echo "processors=$(getconf _NPROCESSORS_ONLN 2>/dev/null || true)"
  echo "OCAMLRUNPARAM=${OCAMLRUNPARAM:-}"
} | tee -a "$report"

append_line ""
append_line "Git worktree state:"
git status --short --branch | tee -a "$report"

case "$suite" in
  quick) alias_name=correctness ;;
  stress) alias_name=stress ;;
esac

if run_gate "$alias_name"; then
  status=0
else
  status=$?
fi

append_line ""
append_line "finished_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
append_line "status=$status"
append_line "report=$report"
ln -sf "$(basename "$report")" "$latest"
append_line "latest=$latest"

exit "$status"
