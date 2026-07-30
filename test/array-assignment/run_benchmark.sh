#!/bin/sh

set -eu

weidu_bin=${1:-./weidu}
case "$weidu_bin" in
  /*) ;;
  *) weidu_bin="$(pwd)/$weidu_bin" ;;
esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
benchmark_root=$(mktemp -d "${TMPDIR:-/tmp}/weidu-array-benchmark.XXXXXX")

cleanup()
{
  if [ -n "${benchmark_root:-}" ] && [ -d "$benchmark_root" ]; then
    rm -rf -- "$benchmark_root"
  fi
}
trap cleanup EXIT HUP INT TERM

cp "$script_dir/benchmark.tp2" "$benchmark_root/test.tp2"
(
  cd "$benchmark_root"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    --log benchmark.log test.tp2 </dev/null
)
cat "$benchmark_root/benchmark.log"
