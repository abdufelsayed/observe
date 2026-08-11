set -eu

runner=$1
trials=${OBSERVE_RACE_TRIALS:-100}
work=${OBSERVE_CONCURRENCY_WORK:-64}

case "$runner" in
  */*) ;;
  *) runner="./$runner" ;;
esac

case "$trials" in
  ''|*[!0-9]*|0)
    printf 'OBSERVE_RACE_TRIALS must be a positive integer\n' >&2
    exit 2
    ;;
esac

case "$work" in
  ''|*[!0-9]*|0)
    printf 'OBSERVE_CONCURRENCY_WORK must be a positive integer\n' >&2
    exit 2
    ;;
esac

trial=0
while [ "$trial" -lt "$trials" ]; do
  "$runner" init-race 8
  trial=$((trial + 1))
done

"$runner" capture-conservation "$work"
"$runner" diagnostic-counting "$work"
