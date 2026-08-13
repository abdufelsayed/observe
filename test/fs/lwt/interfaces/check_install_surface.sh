set -eu

ocamlfind_bin=$1
manifest=$2
core_cmi=$3
fs_cmi=$4
lwt_cmi=$5
positive_source=$6

for required in "$manifest" "$core_cmi" "$fs_cmi" "$lwt_cmi" "$positive_source"; do
  test -f "$required" || { printf 'Lwt FS artifact missing: %s\n' "$required" >&2; exit 1; }
done
grep -Fq '/lib/observe-fs-lwt/observe_fs_lwt.cmi"' "$manifest"
if grep -Eq '/lib/(unix|lwt-unix)' "$manifest"; then
  printf 'observe-fs-lwt manifest contains a Unix package\n' >&2
  exit 1
fi

temporary=$(mktemp -d "${TMPDIR:-/tmp}/observe-fs-lwt-surface.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM
cp "$positive_source" "$temporary/consumer.ml"
"$ocamlfind_bin" ocamlc -package repr,jsonm,lwt -I "$(dirname "$core_cmi")" -I "$(dirname "$fs_cmi")" -I "$(dirname "$lwt_cmi")" -c "$temporary/consumer.ml"
