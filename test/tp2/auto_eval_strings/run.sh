#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../../.." && pwd)
weidu=${1:-"$repo_root/weidu"}

if [[ ! -x "$weidu" ]]; then
  echo "WeiDU executable is not available at $weidu" >&2
  exit 1
fi

weidu=$(cd -- "$(dirname -- "$weidu")" && pwd)/$(basename -- "$weidu")
work_dir=$(mktemp -d)

cleanup() {
  if [[ -n "${work_dir:-}" && -d "$work_dir" ]]; then
    rm -rf -- "$work_dir"
  fi
}
trap cleanup EXIT HUP INT TERM

mkdir -p "$work_dir/auto-eval-strings"
cp -- "$script_dir/compile.baf" "$script_dir/extend.baf" \
  "$work_dir/auto-eval-strings/"
cp -- "$script_dir/setup-auto-eval-strings.tp2" "$work_dir/"

cd -- "$work_dir"
"$weidu" setup-auto-eval-strings.tp2 \
  --nogame \
  --noautoupdate \
  --no-exit-pause \
  --force-install 0

expected_outputs=(
  override/compile.bcs
  override/issue75-top.bcs
  override/issue75-bottom.bcs
  override/issue75-regexp-top.bcs
  override/issue75-regexp-bottom.bcs
)

for output in "${expected_outputs[@]}"; do
  if [[ ! -f "$output" ]]; then
    echo "AUTO_EVAL_STRINGS test did not create $output" >&2
    exit 1
  fi
done
