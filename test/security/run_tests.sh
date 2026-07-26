#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 PATH_TO_WEIDU" >&2
  exit 2
fi

weidu_input=$1
weidu_dir=$(cd "$(dirname "$weidu_input")" && pwd)
weidu="$weidu_dir/$(basename "$weidu_input")"
test_root=$(mktemp -d)
trap 'rm -rf "$test_root"' EXIT

fail() {
  echo "security regression test failed: $*" >&2
  exit 1
}

write_tp2() {
  local destination=$1
  local action=$2
  {
    printf 'BACKUP ~backup~\n'
    printf 'AUTHOR ~WeiDU security regression tests~\n'
    printf 'BEGIN ~authority test~\n'
    printf '%s\n' "$action"
  } > "$destination"
}

run_install() {
  local directory=$1
  shift
  (
    cd "$directory"
    "$weidu" --nogame --yes --no-exit-pause --force-install 0 "$@"
  )
}

run_uninstall() {
  local directory=$1
  shift
  (
    cd "$directory"
    "$weidu" --nogame --yes --no-exit-pause --force-uninstall 0 "$@"
  )
}

require_denial_message() {
  local output=$1
  local description=$2
  if grep -Fq "FILE ACCESS DENIED" "$output"; then
    return
  fi
  printf 'captured WeiDU output follows:\n' >&2
  sed -n '1,200p' "$output" >&2
  fail "$description"
}

mkdir -p "$test_root/outside" "$test_root/read-denied"
printf 'protected\n' > "$test_root/outside/marker"
write_tp2 "$test_root/read-denied/test.tp2" \
  'COPY ~../outside/marker~ ~copied-marker~'
if run_install "$test_root/read-denied" test.tp2 \
    >"$test_root/read-denied/output" 2>&1; then
  fail "an out-of-root read was accepted"
fi
require_denial_message "$test_root/read-denied/output" \
  "the read denial was not reported clearly"
[[ ! -e "$test_root/read-denied/copied-marker" ]] ||
  fail "the denied read produced an output file"

mkdir -p "$test_root/write-denied"
write_tp2 "$test_root/write-denied/test.tp2" \
  'DELETE ~../outside/marker~'
if run_install "$test_root/write-denied" --continue test.tp2 \
    >"$test_root/write-denied/output" 2>&1; then
  fail "--continue suppressed an authority denial"
fi
require_denial_message "$test_root/write-denied/output" \
  "the write denial was not reported clearly"
[[ -f "$test_root/outside/marker" ]] ||
  fail "the out-of-root file was deleted"

mkdir -p "$test_root/symlink-denied"
ln -s ../outside "$test_root/symlink-denied/link"
write_tp2 "$test_root/symlink-denied/test.tp2" \
  'DELETE ~link/marker~'
if run_install "$test_root/symlink-denied" test.tp2 \
    >"$test_root/symlink-denied/output" 2>&1; then
  fail "a symlink escape was accepted"
fi
require_denial_message "$test_root/symlink-denied/output" \
  "the symlink denial was not reported clearly"
[[ -f "$test_root/outside/marker" ]] ||
  fail "the symlink target was deleted"

mkdir -p "$test_root/action-try"
write_tp2 "$test_root/action-try/test.tp2" \
  $'ACTION_TRY\n  DELETE ~../outside/marker~\nWITH\n  DEFAULT\n    COPY ~test.tp2~ ~caught-marker~\nEND'
if run_install "$test_root/action-try" test.tp2 \
    >"$test_root/action-try/output" 2>&1; then
  fail "ACTION_TRY suppressed an authority denial"
fi
require_denial_message "$test_root/action-try/output" \
  "the ACTION_TRY denial was not reported clearly"
[[ -f "$test_root/outside/marker" ]] ||
  fail "ACTION_TRY allowed the out-of-root file to be deleted"
[[ ! -e "$test_root/action-try/caught-marker" ]] ||
  fail "ACTION_TRY continued after an authority denial"

mkdir -p "$test_root/explicit-root"
write_tp2 "$test_root/explicit-root/test.tp2" \
  'DELETE ~../outside/marker~'
run_install "$test_root/explicit-root" \
  --allow-file-root ../outside test.tp2 \
  >"$test_root/explicit-root/output" 2>&1 ||
  fail "an explicitly authorized root was rejected"
[[ ! -e "$test_root/outside/marker" ]] ||
  fail "the explicitly authorized delete did not run"

mkdir -p "$test_root/uninstall-preflight"
printf 'source\n' > "$test_root/uninstall-preflight/source"
write_tp2 "$test_root/uninstall-preflight/test.tp2" \
  'COPY ~source~ ~installed~'
run_install "$test_root/uninstall-preflight" test.tp2 \
  >"$test_root/uninstall-preflight/install-output" 2>&1 ||
  fail "the uninstall preflight fixture could not be installed"
uninstall_metadata="$test_root/uninstall-preflight/backup/0/UNINSTALL.0"
[[ -f "$uninstall_metadata" ]] ||
  fail "the uninstall preflight fixture did not create metadata"
printf '../outside/marker\n' > "$uninstall_metadata"
printf 'protected\n' > "$test_root/outside/marker"
if run_uninstall "$test_root/uninstall-preflight" test.tp2 \
    >"$test_root/uninstall-preflight/uninstall-output" 2>&1; then
  fail "out-of-root uninstall metadata was accepted"
fi
require_denial_message "$test_root/uninstall-preflight/uninstall-output" \
  "the uninstall metadata denial was not reported clearly"
[[ -f "$test_root/outside/marker" ]] ||
  fail "uninstall metadata deleted the out-of-root file"
[[ -f "$test_root/uninstall-preflight/installed" ]] ||
  fail "uninstall mutated files before authority preflight completed"
[[ -f "$uninstall_metadata" ]] ||
  fail "uninstall removed metadata before authority preflight completed"

if [[ $(uname -s) == Linux ]]; then
  mkdir -p "$test_root/autoupdate"
  cp "$weidu" "$test_root/autoupdate/weidu"
  cp "$weidu" "$test_root/autoupdate/setup-peer"
  printf 'different digest\n' >> "$test_root/autoupdate/setup-peer"
  {
    printf '#include <stdio.h>\n'
    printf 'int main(void) { FILE *f = fopen("executed", "wb"); '
    printf 'if (f != NULL) fclose(f); return 0; }\n'
  } > "$test_root/autoupdate/hostile.c"
  "${CC:-cc}" "$test_root/autoupdate/hostile.c" \
    -o "$test_root/autoupdate/setup-hostile"
  {
    printf '#include <stdio.h>\n'
    printf 'static const char version_marker[] = '
    printf '"\\0WEIDU_AUTOUPDATE_VERSION:999999\\0";\n'
    printf 'int main(void) { FILE *f = fopen("executed-newer", "wb"); '
    printf 'if (f != NULL) fclose(f); '
    printf 'fwrite(version_marker, 1, sizeof version_marker, stdout); '
    printf 'return 0; }\n'
  } > "$test_root/autoupdate/hostile-newer.c"
  "${CC:-cc}" "$test_root/autoupdate/hostile-newer.c" \
    -o "$test_root/autoupdate/setup-newer"
  cp "$test_root/autoupdate/setup-newer" \
    "$test_root/autoupdate/newer.expected"
  (
    cd "$test_root/autoupdate"
    ./weidu --update-all >output 2>&1
  ) || fail "safe auto-update failed"
  [[ ! -e "$test_root/autoupdate/executed" ]] ||
    fail "auto-update executed a sibling setup binary"
  cmp -s "$test_root/autoupdate/weidu" \
    "$test_root/autoupdate/setup-hostile" ||
    fail "the unmarked sibling was not replaced from the running binary"
  cmp -s "$test_root/autoupdate/weidu" \
    "$test_root/autoupdate/setup-peer" ||
    fail "the marked peer was not replaced from the running binary"
  [[ ! -e "$test_root/autoupdate/executed-newer" ]] ||
    fail "auto-update executed a newer sibling setup binary"
  cmp -s "$test_root/autoupdate/newer.expected" \
    "$test_root/autoupdate/setup-newer" ||
    fail "the newer marked sibling was overwritten"
fi

echo "security regression tests passed"
