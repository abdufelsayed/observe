set -eu

ocamlfind_bin=$1
manifest=$2
public_cmi=$3
public_mli=$4
public_meta=$5
public_dune_package=$6
positive_source=$7
negative_dir=$8

if [ ! -f "$manifest" ]; then
  printf 'install manifest not found: %s\n' "$manifest" >&2
  exit 1
fi

if ! grep -q '/lib/observe/observe\.cmi"' "$manifest"; then
  printf 'observe.cmi is absent from install manifest\n' >&2
  exit 1
fi

public_root=$(dirname "$public_cmi")

for required in "$public_cmi" "$public_mli" "$public_meta" "$public_dune_package"; do
  if [ ! -f "$required" ]; then
    printf 'installed public artifact missing: %s\n' "$required" >&2
    exit 1
  fi
done

if ! grep -q '/observe/ppx/ppx\.exe"' "$manifest"; then
  printf 'observe.ppx driver is absent from install manifest\n' >&2
  exit 1
fi

while IFS= read -r line; do
  case "$line" in
    *observe__Observe_*.cmi*|*observe__Observe_*.cmti*)
      case "$line" in
        *'/.private/'*) ;;
        *)
          printf 'private Observe CMI is installed publicly: %s\n' "$line" >&2
          exit 1
          ;;
      esac
      ;;
  esac
done <"$manifest"

temporary=$(mktemp -d "${TMPDIR:-/tmp}/observe-installed-surface.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM

compile() {
  source=$1
  output=$2
  isolated_source="$temporary/$(basename "${output%.cmo}").ml"
  cp "$source" "$isolated_source"
  "$ocamlfind_bin" ocamlc -package repr -I "$public_root" -c -impl "$isolated_source" -o "$output"
}

compile "$positive_source" "$temporary/public_consumer.cmo"

expected() {
  case "$1" in
    config_formatter.ml.fail) printf '%s' 'formatter' ;;
    config_terminal.ml.fail) printf '%s' 'terminal' ;;
    mismatched_structured.ml.fail) printf '%s' '(constant|expression) has type' ;;
    private_engine.ml.fail) printf '%s' 'Observe__Observe_engine' ;;
    private_runtime.ml.fail) printf '%s' 'Observe__Observe_runtime' ;;
    untagged_text.ml.fail) printf '%s' 'tag' ;;
    with_tag.ml.fail) printf '%s' 'Unbound value.*Observe.Logs.with_tag' ;;
    *) return 1 ;;
  esac
}

for source in "$negative_dir"/*.ml.fail; do
  name=$(basename "$source")
  pattern=$(expected "$name")
  if compile "$source" "$temporary/${name%.ml.fail}.cmo" 2>"$temporary/$name.err"; then
    printf 'negative interface fixture unexpectedly compiled: %s\n' "$source" >&2
    exit 1
  fi
  if ! grep -E "$pattern" "$temporary/$name.err" >/dev/null; then
    cat "$temporary/$name.err" >&2
    printf 'interface fixture failed for an unexpected reason: %s\n' "$source" >&2
    exit 1
  fi
done
