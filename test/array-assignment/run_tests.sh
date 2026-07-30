#!/bin/sh

set -eu

weidu_bin=${1:-./weidu}
case "$weidu_bin" in
  /*) ;;
  *) weidu_bin="$(pwd)/$weidu_bin" ;;
esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
test_root=$(mktemp -d "${TMPDIR:-/tmp}/weidu-array-assignment.XXXXXX")

cleanup()
{
  if [ -n "${test_root:-}" ] && [ -d "$test_root" ]; then
    rm -rf -- "$test_root"
  fi
}
trap cleanup EXIT HUP INT TERM

cp "$script_dir/correctness.tp2" "$test_root/test.tp2"
if ! (
  cd "$test_root"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    test.tp2 </dev/null >output.log 2>&1
); then
  sed 's/^/| /' "$test_root/output.log" >&2
  exit 1
fi

if ! grep -F "array-assignment tests passed" \
  "$test_root/output.log" >/dev/null; then
  sed 's/^/| /' "$test_root/output.log" >&2
  echo "array-assignment test completion marker was not printed" >&2
  exit 1
fi

echo "array-assignment tests passed"
