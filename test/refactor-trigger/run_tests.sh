#!/bin/sh

set -eu

weidu_bin=${1:-./weidu}
case "$weidu_bin" in
  /*) ;;
  *) weidu_bin="$(pwd)/$weidu_bin" ;;
esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
test_root=$(mktemp -d "${TMPDIR:-/tmp}/weidu-refactor-trigger.XXXXXX")

cleanup()
{
  if [ -n "${test_root:-}" ] && [ -d "$test_root" ]; then
    rm -rf -- "$test_root"
  fi
}
trap cleanup EXIT HUP INT TERM

cp "$script_dir"/*.tp2 "$test_root/"
cp -R "$script_dir/fixtures" "$script_dir/expected" "$test_root/"
mkdir -p "$test_root/results"

canonicalize()
{
  tr '\r' '\n' <"$1" | awk '
    {
      for (i = 1; i <= NF; ++i) {
        if (seen) printf " ";
        printf "%s", $i;
        seen = 1;
      }
    }
    END { print "" }
  ' | sed 's/ (/(/g'
}

failures=0

run_suite()
{
  suite=$1
  shift
  suite_failed=0
  log_file="$test_root/$suite.log"

  if ! (
    cd "$test_root"
    "$weidu_bin" --noautoupdate --nogame --no-exit-pause \
      --force-install 0 "$suite.tp2" </dev/null >"$log_file" 2>&1
  ); then
    sed 's/^/| /' "$log_file" >&2
    echo "$suite installer invocation failed" >&2
    suite_failed=1
  fi

  for case_name do
    actual="$test_root/results/$case_name.baf"
    expected="$test_root/expected/$case_name.baf"
    actual_normalized="$test_root/$case_name.actual"
    expected_normalized="$test_root/$case_name.expected"

    if [ ! -f "$actual" ]; then
      echo "$case_name did not produce $actual" >&2
      suite_failed=1
      continue
    fi

    canonicalize "$actual" >"$actual_normalized"
    canonicalize "$expected" >"$expected_normalized"
    if ! cmp -s "$actual_normalized" "$expected_normalized"; then
      echo "$case_name produced an unexpected trigger sequence" >&2
      diff -u "$expected_normalized" "$actual_normalized" >&2 || true
      suite_failed=1
    fi
  done

  if [ "$suite_failed" -eq 0 ]; then
    echo "$suite: PASS"
  else
    echo "$suite: FAIL" >&2
    failures=$((failures + 1))
  fi
}

run_suite issue-95 \
  issue-95-same \
  issue-95-different \
  issue-95-positive

run_suite issue-96 \
  issue-96-simple \
  issue-96-conjunction \
  issue-96-or \
  issue-96-negated-simple \
  issue-96-negated-conjunction \
  issue-96-negated-or \
  issue-96-negated-explicit \
  issue-96-backreference

if [ "$failures" -ne 0 ]; then
  exit 1
fi

echo "refactor-trigger tests passed"
