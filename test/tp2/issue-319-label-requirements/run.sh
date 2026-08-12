#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/weidu" >&2
  exit 2
fi

weidu=$(realpath "$1")
fixture_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
test_root=$(mktemp -d)
trap 'rm -rf -- "$test_root"' EXIT

new_case() {
  local name=$1
  local case_dir="$test_root/$name"

  mkdir -p "$case_dir"
  cp "$fixture_dir"/*.tp2 "$case_dir/"
  printf '%s\n' "$case_dir"
}

run_weidu() {
  local case_dir=$1
  shift
  (
    cd "$case_dir"
    "$weidu" --nogame --noautoupdate --no-exit-pause "$@"
  )
}

component_is_installed() {
  local case_dir=$1
  local tp2=$2
  local component=$3

  [[ -f "$case_dir/WeiDU.log" ]] &&
    grep -Eq "^~${tp2}~ #0 #${component}([[:space:]]|$)" \
      "$case_dir/WeiDU.log"
}

assert_installed() {
  local case_dir=$1
  local tp2=$2
  local component=$3

  if ! component_is_installed "$case_dir" "$tp2" "$component"; then
    echo "Expected ${tp2} component ${component} to be installed" >&2
    [[ ! -f "$case_dir/WeiDU.log" ]] || sed -n '1,200p' "$case_dir/WeiDU.log" >&2
    exit 1
  fi
}

assert_not_installed() {
  local case_dir=$1
  local tp2=$2
  local component=$3

  if component_is_installed "$case_dir" "$tp2" "$component"; then
    echo "Expected ${tp2} component ${component} not to be installed" >&2
    sed -n '1,200p' "$case_dir/WeiDU.log" >&2
    exit 1
  fi
}

missing_case=$(new_case missing)
run_weidu "$missing_case" consumer.tp2 --force-install 0 \
  >"$missing_case/require-missing.out" 2>&1
assert_not_installed "$missing_case" CONSUMER.TP2 0
grep -Fq "The shared feature is required" "$missing_case/require-missing.out"

run_weidu "$missing_case" consumer.tp2 --force-install 3
assert_installed "$missing_case" CONSUMER.TP2 3

provider_a_case=$(new_case provider-a)
run_weidu "$provider_a_case" provider-a.tp2 --force-install 0
run_weidu "$provider_a_case" consumer.tp2 --force-install 0
assert_installed "$provider_a_case" CONSUMER.TP2 0

run_weidu "$provider_a_case" consumer.tp2 --force-install 2 \
  >"$provider_a_case/forbid-present.out" 2>&1
assert_not_installed "$provider_a_case" CONSUMER.TP2 2
grep -Fq "The shared feature is forbidden" "$provider_a_case/forbid-present.out"

provider_b_case=$(new_case provider-b)
run_weidu "$provider_b_case" provider-b.tp2 --force-install 0
run_weidu "$provider_b_case" consumer.tp2 --force-install 0
assert_installed "$provider_b_case" CONSUMER.TP2 0

echo "Issue #319 label requirement tests passed"
