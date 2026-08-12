set -eu

manifest=$1
installed_suffix=$2

if [ ! -f "$manifest" ]; then
  printf 'install manifest not found: %s\n' "$manifest" >&2
  exit 1
fi

if ! grep -Fq "$installed_suffix" "$manifest"; then
  printf 'public artifact is absent from install manifest: %s\n' \
    "$installed_suffix" >&2
  exit 1
fi
