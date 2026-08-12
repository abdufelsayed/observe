set -eu

ocamlfind_bin=$1
shift
ready_manifest=$1
core_cmi=$2
lwt_cmi=$3
ready_cmi=$4
positive_source=$5
negative_dir=$6

for required in \
  "$ready_manifest" \
  "$core_cmi" \
  "$lwt_cmi" \
  "$ready_cmi"
do
  if [ ! -f "$required" ]; then
    printf 'installed Lwt-Unix artifact missing: %s\n' "$required" >&2
    exit 1
  fi
done

if ! grep -Fq '/lib/observe-lwt-unix/observe_lwt_unix.cmi"' "$ready_manifest"; then
  printf 'observe-lwt-unix CMI is absent from install manifest\n' >&2
  exit 1
fi

temporary=$(mktemp -d "${TMPDIR:-/tmp}/observe-lwt-unix-surface.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM

compile() {
  source=$1
  output=$2
  isolated_source="$temporary/$(basename "${output%.cmo}").ml"
  cp "$source" "$isolated_source"
  "$ocamlfind_bin" ocamlc -thread \
    -package repr,lwt,lwt.unix,ptime.clock,unix \
    -I "$(dirname "$core_cmi")" \
    -I "$(dirname "$lwt_cmi")" \
    -I "$(dirname "$ready_cmi")" \
    -c -impl "$isolated_source" -o "$output"
}

compile "$positive_source" "$temporary/adapter_consumer.cmo"

for source in "$negative_dir"/*.ml.fail; do
  name=$(basename "$source")
  if compile "$source" "$temporary/${name%.ml.fail}.cmo" 2>"$temporary/$name.err"; then
    printf 'private Lwt-Unix fixture unexpectedly compiled: %s\n' "$source" >&2
    exit 1
  fi
  if ! grep -E 'Unbound module' "$temporary/$name.err" >/dev/null; then
    cat "$temporary/$name.err" >&2
    printf 'private Lwt-Unix fixture failed unexpectedly: %s\n' "$source" >&2
    exit 1
  fi
done
