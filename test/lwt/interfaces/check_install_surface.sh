set -eu

ocamlfind_bin=$1
manifest=$2
core_cmi=$3
lwt_cmi=$4
positive_source=$5

if [ ! -f "$manifest" ]; then
  printf 'install manifest not found: %s\n' "$manifest" >&2
  exit 1
fi

if ! grep -Fq '/lib/observe-lwt/observe_lwt.cmi"' "$manifest"; then
  printf 'observe-lwt CMI is absent from install manifest\n' >&2
  exit 1
fi

for required in "$core_cmi" "$lwt_cmi" "$positive_source"; do
  if [ ! -f "$required" ]; then
    printf 'installed Lwt artifact missing: %s\n' "$required" >&2
    exit 1
  fi
done

temporary=$(mktemp -d "${TMPDIR:-/tmp}/observe-lwt-surface.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM

isolated_source="$temporary/consumer.ml"
cp "$positive_source" "$isolated_source"
"$ocamlfind_bin" ocamlc \
  -package repr,lwt \
  -I "$(dirname "$core_cmi")" \
  -I "$(dirname "$lwt_cmi")" \
  -c -impl "$isolated_source" -o "$temporary/consumer.cmo"
