set -eu

ocamlfind_bin=$1
manifest=$2
core_cmi=$3
ready_cmi=$4
positive_source=$5
negative_dir=$6

for required in "$manifest" "$core_cmi" "$ready_cmi" "$positive_source"; do
  test -f "$required" || { printf 'ready FS artifact missing: %s\n' "$required" >&2; exit 1; }
done
grep -Fq '/lib/observe-fs-lwt-unix/observe_fs_lwt_unix.cmi"' "$manifest"

temporary=$(mktemp -d "${TMPDIR:-/tmp}/observe-fs-lwt-unix-surface.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM
compile() {
  source=$1
  output=$2
  isolated="$temporary/$(basename "${output%.cmo}").ml"
  cp "$source" "$isolated"
  "$ocamlfind_bin" ocamlc -thread -package repr,jsonm,lwt,lwt.unix,unix -I "$(dirname "$core_cmi")" -I "$(dirname "$ready_cmi")" -c "$isolated" -o "$output"
}
compile "$positive_source" "$temporary/consumer.cmo"
for source in "$negative_dir"/*.ml.fail; do
  if compile "$source" "$temporary/private.cmo" 2>"$temporary/error"; then
    printf 'private ready FS module compiled: %s\n' "$source" >&2
    exit 1
  fi
  grep -Eq 'Unbound module' "$temporary/error"
done
