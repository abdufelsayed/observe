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
  participants=$((2 + trial % 15))
  "$runner" init-race "$participants"
  trial=$((trial + 1))
done

half=$((work / 2))
if [ "$half" -lt 1 ]; then
  half=1
fi

"$runner" capture-conservation 1 1
"$runner" capture-conservation 8 1
"$runner" capture-conservation 8 3
"$runner" capture-conservation "$work" 1
"$runner" capture-conservation "$work" "$half"
"$runner" capture-conservation "$work" "$work"
"$runner" capture-conservation "$work" "$((work + 3))"
"$runner" diagnostic-counting 1
"$runner" diagnostic-counting 8
"$runner" diagnostic-counting "$work"
"$runner" wide-contribution-and-seal "$work"
"$runner" wide-set-level-emit-race "$work"
"$runner" wide-authoring-linearization
"$runner" wide-parallel-materialization
"$runner" terminal-race "$work"
