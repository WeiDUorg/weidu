#!/bin/sh

set -eu

weidu_bin=${1:-./weidu}
case "$weidu_bin" in
  /*) ;;
  *) weidu_bin="$(pwd)/$weidu_bin" ;;
esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
test_root=$(mktemp -d "${TMPDIR:-/tmp}/weidu-command-policy.XXXXXX")
prompt_pid=

cleanup()
{
  if [ -n "${prompt_pid:-}" ]; then
    kill "$prompt_pid" 2>/dev/null || true
    wait "$prompt_pid" 2>/dev/null || true
  fi
  if [ -n "${test_root:-}" ] && [ -d "$test_root" ]; then
    rm -rf -- "$test_root"
  fi
}
trap cleanup EXIT HUP INT TERM

fail()
{
  echo "external-command-policy test failed: $*" >&2
  exit 1
}

fail_case()
{
  failed_case_dir=$1
  shift
  for failed_log in output.log install.log; do
    if [ -f "$failed_case_dir/$failed_log" ]; then
      echo "----- captured $failed_log -----" >&2
      sed 's/^/| /' "$failed_case_dir/$failed_log" >&2
      echo "----- end captured $failed_log -----" >&2
    fi
  done
  fail "$@"
}

prepare_case()
{
  case_name=$1
  fixture=$2
  case_dir="$test_root/$case_name"
  mkdir "$case_dir"
  cp "$script_dir/$fixture" "$case_dir/test.tp2"
}

assert_absent()
{
  assertion_case_dir=$1
  assertion_path=$2
  assertion_message=$3
  [ ! -e "$assertion_case_dir/$assertion_path" ] ||
    fail_case "$assertion_case_dir" "$assertion_message"
}

assert_present()
{
  assertion_case_dir=$1
  assertion_path=$2
  assertion_message=$3
  [ -e "$assertion_case_dir/$assertion_path" ] ||
    fail_case "$assertion_case_dir" "$assertion_message"
}

assert_output()
{
  assertion_case_dir=$1
  assertion_text=$2
  assertion_message=$3
  grep -F "$assertion_text" "$assertion_case_dir/output.log" >/dev/null ||
    fail_case "$assertion_case_dir" "$assertion_message"
}

assert_denied()
{
  case_name=$1
  marker=$2
  shift 2
  case_dir="$test_root/$case_name"
  if (
    cd "$case_dir"
    "$weidu_bin" --nogame --no-exit-pause --force-install 0 "$@" \
      test.tp2 </dev/null >output.log 2>&1
  ); then
    fail_case "$case_dir" "$case_name unexpectedly succeeded"
  fi
  assert_absent "$case_dir" "$marker" \
    "$case_name executed the denied command"
  assert_output "$case_dir" "External command denied by security policy" \
    "$case_name did not report the denial"
}

start_prompt_case()
{
  prompt_case_name=$1
  shift
  case_dir="$test_root/$prompt_case_name"
  input_fifo="$case_dir/input.fifo"
  mkfifo "$input_fifo"
  (
    cd "$case_dir"
    "$weidu_bin" --nogame --no-exit-pause "$@" \
      test.tp2 <input.fifo >output.log 2>&1
  ) &
  prompt_pid=$!
  exec 3>"$input_fifo"
}

wait_for_confirmation_token()
{
  token_index=$1
  token_tries=0
  while [ "$token_tries" -lt 30 ]; do
    confirmation_token=$(
      sed -n 's/^Confirmation token: \([A-Z2-9][A-Z2-9]*\)$/\1/p' \
        "$case_dir/output.log" | sed -n "${token_index}p"
    )
    if [ -n "$confirmation_token" ]; then
      printf '%s\n' "$confirmation_token"
      return
    fi
    if ! kill -0 "$prompt_pid" 2>/dev/null; then
      fail_case "$case_dir" \
        "prompt process exited before confirmation token $token_index"
    fi
    token_tries=$((token_tries + 1))
    sleep 1
  done
  fail_case "$case_dir" \
    "timed out waiting for confirmation token $token_index"
}

send_prompt_answer()
{
  prompt_choice=$1
  prompt_token=${2:-}
  if [ -n "$prompt_token" ]; then
    printf '%s %s\n' "$prompt_choice" "$prompt_token" >&3
  else
    printf '%s\n' "$prompt_choice" >&3
  fi
}

finish_prompt_case()
{
  exec 3>&-
  if wait "$prompt_pid"; then
    prompt_status=0
  else
    prompt_status=$?
  fi
  prompt_pid=
}

prepare_case default-deny at-now.tp2
assert_denied default-deny command-ran

prepare_case explicit-deny at-now.tp2
assert_denied explicit-deny command-ran --deny-external-commands

prepare_case yes-does-not-authorize at-now.tp2
assert_denied yes-does-not-authorize command-ran --yes

prepare_case prompt-deny at-now.tp2
case_dir="$test_root/prompt-deny"
if (
  cd "$case_dir"
  printf 'N\n' |
    "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
      --ask-external-commands test.tp2 >output.log 2>&1
); then
  fail_case "$case_dir" "prompt-deny unexpectedly succeeded"
fi
assert_absent "$case_dir" command-ran \
  "prompt-deny executed the denied command"
assert_output "$case_dir" 'Requesting TP2: "test.tp2"' \
  "prompt-deny omitted the requesting TP2"
assert_output "$case_dir" "Component: 0" \
  "prompt-deny omitted the component number"
assert_output "$case_dir" "Action: AT_NOW" \
  "prompt-deny omitted the action type"
assert_output "$case_dir" "Confirmation token:" \
  "prompt-deny omitted the fresh confirmation token"

prepare_case explicit-allow at-now.tp2
case_dir="$test_root/explicit-allow"
(
  cd "$case_dir"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    --allow-external-commands test.tp2 </dev/null >output.log 2>&1
)
assert_present "$case_dir" command-ran \
  "explicit-allow did not execute the authorized command"

prepare_case non-exact non-exact.tp2
case_dir="$test_root/non-exact"
(
  cd "$case_dir"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    --allow-external-commands test.tp2 </dev/null >output.log 2>&1
)
assert_present "$case_dir" command-ran \
  "non-EXACT command did not execute after authorization"

prepare_case action-try action-try.tp2
printf 'source\n' >"$test_root/action-try/source.txt"
assert_denied action-try denied-command-ran
assert_absent "$test_root/action-try" action-try-caught \
  "ACTION_TRY caught and downgraded command denial"
assert_absent "$test_root/action-try" continued-after-action-try \
  "processing continued after ACTION_TRY command denial"

prepare_case continue continue.tp2
printf 'source\n' >"$test_root/continue/source.txt"
assert_denied continue denied-command-ran --continue
assert_absent "$test_root/continue" continued-after-denial \
  "--continue downgraded command denial"

prepare_case deferred-deny at-exit.tp2
assert_denied deferred-deny command-ran

prepare_case deferred-allow at-exit.tp2
case_dir="$test_root/deferred-allow"
(
  cd "$case_dir"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    --allow-external-commands test.tp2 </dev/null >output.log 2>&1
)
assert_present "$case_dir" command-ran \
  "deferred-allow did not execute the authorized command"

prepare_case readme-deny readme.tp2
printf 'readme\n' >"$test_root/readme-deny/readme.txt"
case_dir="$test_root/readme-deny"
if (
  cd "$case_dir"
  printf 'Y\nN\n' |
    "$weidu_bin" --nogame --no-exit-pause --ask-external-commands \
      test.tp2 >output.log 2>&1
); then
  fail_case "$case_dir" "readme-deny unexpectedly succeeded"
fi
assert_output "$case_dir" "Action: README" \
  "README authorization omitted the action type"
assert_output "$case_dir" "Component: not applicable" \
  "README authorization reported a stale component"

prepare_case uninstall-restoration uninstall-state.tp2
case_dir="$test_root/uninstall-restoration"
printf 'original\n' >"$case_dir/target.txt"
printf 'replacement\n' >"$case_dir/replacement.txt"
printf 'moved\n' >"$case_dir/move-source.txt"
(
  cd "$case_dir"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    test.tp2 </dev/null >install.log 2>&1
)
grep -Fx "replacement" "$case_dir/target.txt" >/dev/null ||
  fail_case "$case_dir" "uninstall fixture did not install replacement content"
assert_absent "$case_dir" move-source.txt \
  "uninstall fixture did not install its MOVE action"
assert_present "$case_dir" move-destination.txt \
  "uninstall fixture did not create the MOVE destination"
: >"$case_dir/backup/0/UNSETSTR.0"
if (
  cd "$case_dir"
  "$weidu_bin" --nogame --no-exit-pause --force-uninstall 0 \
    --deny-external-commands --continue \
    test.tp2 </dev/null >output.log 2>&1
); then
  fail_case "$case_dir" "denied uninstall unexpectedly succeeded"
fi
assert_absent "$case_dir" uninstall-command-ran \
  "denied uninstall hook executed"
grep -Fx "original" "$case_dir/target.txt" >/dev/null ||
  fail_case "$case_dir" "COPY restoration did not run after hook denial"
grep -Fx "moved" "$case_dir/move-source.txt" >/dev/null ||
  fail_case "$case_dir" "MOVE restoration did not run after hook denial"
assert_absent "$case_dir" move-destination.txt \
  "MOVE destination remained after hook denial"
assert_absent "$case_dir" backup/0/UNSETSTR.0 \
  "STRSET restoration did not run after hook denial"
if grep -E '^~TEST\.TP2~[[:space:]]+#0[[:space:]]+#0' \
    "$case_dir/WeiDU.log" >/dev/null; then
  fail_case "$case_dir" "WeiDU.log still records restored files as installed"
fi
grep -F "// Recently Uninstalled: ~TEST.TP2~ #0 #0" \
  "$case_dir/WeiDU.log" >/dev/null ||
  fail_case "$case_dir" "WeiDU.log did not record the restored component state"
if grep -F "SUCCESSFULLY REMOVED" "$case_dir/output.log" >/dev/null; then
  fail_case "$case_dir" "denied uninstall reported false success"
fi

case_dir="$test_root/uninstall-action-try"
mkdir -p "$case_dir"
cp "$script_dir/uninstall-action-if.tp2" "$case_dir/victim.tp2"
cp "$script_dir/uninstall-action-try.tp2" "$case_dir/test.tp2"
printf 'source\n' >"$case_dir/source.txt"
printf 'original\n' >"$case_dir/target.txt"
printf 'replacement\n' >"$case_dir/replacement.txt"
printf 'moved\n' >"$case_dir/move-source.txt"
(
  cd "$case_dir"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    victim.tp2 </dev/null >install.log 2>&1
)
if (
  cd "$case_dir"
  "$weidu_bin" --nogame --no-exit-pause --force-install 0 \
    --deny-external-commands --continue \
    test.tp2 </dev/null >output.log 2>&1
); then
  fail_case "$case_dir" \
    "ACTION_TRY around a denied uninstall unexpectedly succeeded"
fi
assert_absent "$case_dir" uninstall-action-try-caught \
  "ACTION_TRY caught and downgraded an uninstall-hook denial"
assert_absent "$case_dir" continued-after-uninstall-denial \
  "--continue downgraded an uninstall-hook denial"
grep -Fx "original" "$case_dir/target.txt" >/dev/null ||
  fail_case "$case_dir" \
    "nested UNINSTALL did not restore files after hook denial"
if grep -E '^~VICTIM\.TP2~[[:space:]]+#0[[:space:]]+#0' \
    "$case_dir/WeiDU.log" >/dev/null; then
  fail_case "$case_dir" \
    "nested UNINSTALL left the restored victim marked as installed"
fi

if [ "${WEIDU_OS:-}" != "win32" ]; then
  prepare_case prompt-allow-once at-now.tp2
  start_prompt_case prompt-allow-once --force-install 0 \
    --ask-external-commands
  first_token=$(wait_for_confirmation_token 1)
  send_prompt_answer Y "$first_token"
  finish_prompt_case
  [ "$prompt_status" -eq 0 ] ||
    fail_case "$case_dir" "prompt-allow-once failed"
  assert_present "$case_dir" command-ran \
    "prompt-allow-once did not execute the authorized command"

  prepare_case prompt-allow-once-then-deny at-now-twice.tp2
  start_prompt_case prompt-allow-once-then-deny --force-install 0 \
    --ask-external-commands
  first_token=$(wait_for_confirmation_token 1)
  send_prompt_answer Y "$first_token"
  second_token=$(wait_for_confirmation_token 2)
  send_prompt_answer N
  finish_prompt_case
  [ "$prompt_status" -ne 0 ] ||
    fail_case "$case_dir" "Y-once followed by N unexpectedly succeeded"
  assert_present "$case_dir" first-command-ran \
    "Y-once did not execute the first command"
  assert_absent "$case_dir" second-command-ran \
    "Y-once authorized the second command"

  prepare_case prompt-component-scope scope.tp2
  start_prompt_case prompt-component-scope --force-install 0 \
    --force-install 1 --ask-external-commands
  first_token=$(wait_for_confirmation_token 1)
  send_prompt_answer A SPOOFED
  send_prompt_answer A "$first_token"
  second_token=$(wait_for_confirmation_token 2)
  send_prompt_answer N
  finish_prompt_case
  [ "$prompt_status" -ne 0 ] ||
    fail_case "$case_dir" "component-scope denial unexpectedly succeeded"
  assert_present "$case_dir" first-command-ran \
    "component approval did not execute the first command"
  assert_present "$case_dir" second-command-ran \
    "component approval did not cover the same component"
  assert_absent "$case_dir" third-command-ran \
    "component approval leaked into another component"
  token_count=$(grep -c '^Confirmation token:' "$case_dir/output.log")
  [ "$token_count" -eq 2 ] ||
    fail_case "$case_dir" \
      "component-scoped approval produced $token_count prompts instead of 2"
  assert_output "$case_dir" "The confirmation did not match." \
    "a mod-supplied fake token was accepted"
  assert_output "$case_dir" "Component: 0" \
    "mod code changed the security prompt's component provenance"
  assert_output "$case_dir" "Component: 1" \
    "second component prompt omitted its provenance"
fi

echo "external-command-policy tests passed"
