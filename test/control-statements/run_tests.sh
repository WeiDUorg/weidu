#!/bin/sh

set -eu

weidu_bin=${1:-./weidu}
case "$weidu_bin" in
  /*) ;;
  *) weidu_bin="$(pwd)/$weidu_bin" ;;
esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
test_root=$(mktemp -d "${TMPDIR:-/tmp}/weidu-control-statements.XXXXXX")

cleanup()
{
  if [ -n "${test_root:-}" ] && [ -d "$test_root" ]; then
    rm -rf -- "$test_root"
  fi
}
trap cleanup EXIT HUP INT TERM

cp "$script_dir"/*.tp2 "$test_root/"
mkdir "$test_root/fixtures"
: >"$test_root/fixtures/alpha.txt"
: >"$test_root/fixtures/beta.txt"

if ! (
  cd "$test_root"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    control-statements.tp2 </dev/null >valid.log 2>&1
); then
  sed 's/^/| /' "$test_root/valid.log" >&2
  exit 1
fi

if ! grep -F "control-statement tests passed" \
  "$test_root/valid.log" >/dev/null; then
  sed 's/^/| /' "$test_root/valid.log" >&2
  echo "control-statement completion marker was not printed" >&2
  exit 1
fi

expect_rejection()
{
  test_file=$1
  expected=$2
  log_file="$test_root/${test_file%.tp2}.log"

  (
    cd "$test_root"
    "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
      "$test_file" </dev/null >"$log_file" 2>&1
  ) || :

  if ! sed 's/\\"/"/g' "$log_file" | grep -F "$expected" >/dev/null; then
    sed 's/^/| /' "$log_file" >&2
    echo "$test_file did not report: $expected" >&2
    exit 1
  fi
}

expect_parse_rejection()
{
  test_file=$1
  log_file="$test_root/${test_file%.tp2}.log"

  (
    cd "$test_root"
    "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
      "$test_file" </dev/null >"$log_file" 2>&1
  ) || :

  if ! grep -F "PARSE ERROR" "$log_file" >/dev/null; then
    sed 's/^/| /' "$log_file" >&2
    echo "$test_file was not rejected by the parser" >&2
    exit 1
  fi
}

expect_rejection invalid-action-continue.tp2 \
  "CONTINUE is not inside an action loop"
expect_rejection invalid-patch-continue.tp2 \
  "CONTINUE is not inside a patch loop"
expect_rejection invalid-cross-domain-continue.tp2 \
  "CONTINUE is not inside a patch loop"
expect_rejection invalid-macro-continue.tp2 \
  "CONTINUE is not inside an action loop"
expect_rejection invalid-outer-for-initializer-continue.tp2 \
  "CONTINUE is not inside a patch loop"
expect_rejection invalid-outer-for-increment-continue.tp2 \
  "CONTINUE is not inside a patch loop"
expect_rejection invalid-patch-for-initializer-continue.tp2 \
  "CONTINUE is not inside a patch loop"
expect_rejection invalid-patch-for-increment-continue.tp2 \
  "CONTINUE is not inside a patch loop"
expect_rejection invalid-action-break.tp2 \
  "BREAK is not inside an action loop"
expect_rejection invalid-patch-break.tp2 \
  "BREAK is not inside a patch loop"
expect_rejection invalid-macro-break.tp2 \
  'BREAK is not inside an action loop'
expect_parse_rejection invalid-goto.tp2
expect_parse_rejection invalid-label.tp2

echo "control-statement tests passed"
