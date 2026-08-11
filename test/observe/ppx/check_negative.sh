set -eu

ppx_driver=$1
source=$2
expected=$3

case "$ppx_driver" in
  */*) ;;
  *) ppx_driver="./$ppx_driver" ;;
esac

temporary=$(mktemp -d "${TMPDIR:-/tmp}/observe-ppx-negative.XXXXXX")
trap 'rm -rf "$temporary"' EXIT HUP INT TERM

if "$ppx_driver" --impl "$source" >"$temporary/out" 2>"$temporary/err"; then
  printf 'negative PPX fixture unexpectedly expanded: %s\n' "$source" >&2
  exit 1
fi

if ! grep -F "$expected" "$temporary/err" >/dev/null; then
  cat "$temporary/err" >&2
  printf 'PPX fixture failed for an unexpected reason: %s\n' "$source" >&2
  exit 1
fi

