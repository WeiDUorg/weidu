#!/bin/sh

set -eu

if [ "$#" -ne 1 ]; then
  echo "usage: $0 /absolute/path/to/weidu" >&2
  exit 2
fi

weidu=$1
case "$weidu" in
  /*) ;;
  *)
    echo "the WeiDU executable path must be absolute" >&2
    exit 2
    ;;
esac

test_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

mkdir "$work_dir/provider" "$work_dir/consumer"
cp "$test_dir/provider.tp2" "$work_dir/provider/provider.tp2"
cp "$test_dir/english.tra" "$work_dir/provider/english.tra"
cp "$test_dir/test.tra" "$work_dir/provider/test.tra"
cp "$test_dir/consumer.tp2" "$work_dir/consumer/consumer.tp2"
cp "$test_dir/input.txt" "$work_dir/consumer/input.txt"

cd "$work_dir"

# Install component zero in the provider's second language with quick-log
# output. Install component one only after replacing the retained TP2 and TRA
# files with version two; both install-time values must survive independently.
"$weidu" --nogame --noautoupdate --no-auto-tp2 --quick-log --language 1 \
  --force-install 0 provider/provider.tp2

grep -F '#WEIDU_VERSION' WeiDU.log >/dev/null

cp "$test_dir/provider-v2.tp2" "$work_dir/provider/provider.tp2"
cp "$test_dir/english-v2.tra" "$work_dir/provider/english.tra"
cp "$test_dir/test-v2.tra" "$work_dir/provider/test.tra"

"$weidu" --nogame --noautoupdate --no-auto-tp2 --language 1 \
  --force-install 1 provider/provider.tp2

"$weidu" --nogame --noautoupdate --no-auto-tp2 \
  --force-install 0 consumer/consumer.tp2

grep -F 'provider installed-language version one' WeiDU.log >/dev/null
grep -F 'provider installed-language version two' WeiDU.log >/dev/null
grep -F 'consumer version must remain isolated' WeiDU.log >/dev/null
cmp "$work_dir/consumer/input.txt" "$work_dir/consumer/output.txt"

# A pre-feature log, or one rewritten by a WeiDU version that does not preserve
# the comment, has no machine-readable install-time version metadata. The query
# must report an unknown version as empty instead of evaluating the current
# retained TP2 and presenting that value as historical fact.
sed 's/ \/\/ #WEIDU_VERSION [0-9A-Fa-f]*//' WeiDU.log > WeiDU.log.legacy
mv WeiDU.log.legacy WeiDU.log

"$weidu" --nogame --noautoupdate --no-auto-tp2 \
  --force-install 1 consumer/consumer.tp2

cmp "$work_dir/consumer/input.txt" "$work_dir/consumer/legacy-output.txt"
