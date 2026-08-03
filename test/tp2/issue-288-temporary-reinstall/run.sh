#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/weidu" >&2
  exit 2
fi

weidu=$(realpath "$1")
fixture_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
test_dir=$(mktemp -d)
trap 'rm -rf -- "$test_dir"' EXIT

cp "$fixture_dir/prerequisite.tp2" "$test_dir/prerequisite.tp2"
cp "$fixture_dir/dependent.tp2" "$test_dir/dependent.tp2"
cd "$test_dir"

run_weidu() {
  "$weidu" --nogame --noautoupdate --no-exit-pause "$@"
}

component_is_installed() {
  local tp2=$1
  local component=$2
  grep -Eq "^~${tp2}~ #0 #${component}([[:space:]]|$)" WeiDU.log
}

fail_with_log() {
  local message=$1
  echo "$message" >&2
  echo "Current WeiDU.log:" >&2
  sed -n '1,200p' WeiDU.log >&2
  exit 1
}

assert_installed() {
  local tp2=$1
  local component=$2
  component_is_installed "$tp2" "$component" ||
    fail_with_log "Expected ${tp2} component ${component} to be installed"
}

assert_not_installed() {
  local tp2=$1
  local component=$2
  if component_is_installed "$tp2" "$component"; then
    fail_with_log "Expected ${tp2} component ${component} not to be installed"
  fi
}

run_weidu prerequisite.tp2 --force-install 0
run_weidu dependent.tp2 --force-install 0
run_weidu prerequisite.tp2 --force-install 1
run_weidu dependent.tp2 --force-install 1

assert_installed PREREQUISITE.TP2 0
assert_installed DEPENDENT.TP2 0
assert_installed PREREQUISITE.TP2 1
assert_installed DEPENDENT.TP2 1

run_weidu prerequisite.tp2 --force-uninstall 0

assert_not_installed PREREQUISITE.TP2 0
assert_installed DEPENDENT.TP2 0
assert_installed PREREQUISITE.TP2 1
assert_not_installed DEPENDENT.TP2 1
