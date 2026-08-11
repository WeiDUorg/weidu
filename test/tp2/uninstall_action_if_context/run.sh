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

cp -- "$script_dir/outer.tp2" "$script_dir/inner.tp2" "$work_dir/"
cd -- "$work_dir"

common_args=(--nogame --noautoupdate --no-exit-pause)

"$weidu" outer.tp2 "${common_args[@]}" --force-install 0 \
  --log outer-install.log
"$weidu" inner.tp2 "${common_args[@]}" --force-install 0 \
  --log inner-install.log
"$weidu" outer.tp2 "${common_args[@]}" --force-uninstall 0 \
  --log outer-uninstall.log

# Removing the older component must temporarily uninstall the newer one.
grep -Fq "[INNER.TP2] component 0" outer-uninstall.log

if [[ ! -f uninstall-branch-ran || -e wrong-branch-ran ]]; then
  echo "Stack uninstall did not select the expected guarded AT_UNINSTALL" >&2
  exit 1
fi

if grep -Fq "GAME_INCLUDES has no rule for %TARGET_GAME%" \
    outer-uninstall.log; then
  echo "Stack uninstall evaluated an installation-only ACTION_IF" >&2
  exit 1
fi
