#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/weidu" >&2
  exit 2
fi

case "$1" in
  /*) weidu=$1 ;;
  *) weidu="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")" ;;
esac

if [[ ! -f "$weidu" ]]; then
  echo "WeiDU executable was not found: $weidu" >&2
  exit 2
fi

source_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT

cp -R "$source_dir"/. "$work_dir"/
mkdir -p "$work_dir/override"

(
  cd "$work_dir"
  "$weidu" --nogame --noautoupdate --yes --force-install 0 regression.tp2
)

cmp "$work_dir/test/expected.bcs" "$work_dir/override/result.bcs"
grep -F '"GLOBALGlobalVar9"' "$work_dir/override/mismatch.bcs" >/dev/null
