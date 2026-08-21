set -eu

ocamlfind_bin=$1
ppx_driver=$2
public_cmi=$3
source=$4
expected=$5

case "$ppx_driver" in
  */*) ;;
  *) ppx_driver="./$ppx_driver" ;;
esac

temporary=$(mktemp -d "${TMPDIR:-/tmp}/observe-ppx-typing.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM

public_root=$(dirname "$public_cmi")
isolated="$temporary/negative.ml"
cp "$source" "$isolated"

if "$ocamlfind_bin" ocamlc -package repr -I "$public_root" \
  -ppx "$ppx_driver --as-ppx" -c -impl "$isolated" \
  -o "$temporary/negative.cmo" 2>"$temporary/error"; then
  printf 'negative typing fixture unexpectedly compiled: %s\n' "$source" >&2
  exit 1
fi

if ! grep -E "$expected" "$temporary/error" >/dev/null; then
  cat "$temporary/error" >&2
  printf 'typing fixture failed for an unexpected reason: %s\n' "$source" >&2
  exit 1
fi
