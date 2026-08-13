set -eu

ocamlfind_bin=$1
manifest=$2
core_cmi=$3
fs_cmi=$4
positive_source=$5
negative_dir=$6

for required in "$manifest" "$core_cmi" "$fs_cmi" "$positive_source"; do
  test -f "$required" || { printf 'portable FS artifact missing: %s\n' "$required" >&2; exit 1; }
done
grep -Fq '/lib/observe-fs/observe_fs.cmi"' "$manifest"
if grep -Eq '/lib/(lwt|unix)' "$manifest"; then
  printf 'portable observe-fs manifest contains an effect package\n' >&2
  exit 1
fi

temporary=$(mktemp -d "${TMPDIR:-/tmp}/observe-fs-surface.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM
cp "$positive_source" "$temporary/consumer.ml"
"$ocamlfind_bin" ocamlc -package repr,jsonm -I "$(dirname "$core_cmi")" -I "$(dirname "$fs_cmi")" -c "$temporary/consumer.ml"

for source in "$negative_dir"/*.ml.fail; do
  isolated="$temporary/$(basename "${source%.ml.fail}").ml"
  cp "$source" "$isolated"
  if "$ocamlfind_bin" ocamlc -package repr,jsonm -I "$(dirname "$core_cmi")" -I "$(dirname "$fs_cmi")" -c "$isolated" 2>"$temporary/error"; then
    printf 'private portable FS module compiled: %s\n' "$source" >&2
    exit 1
  fi
  grep -Eq 'Unbound module' "$temporary/error"
done
