#!/bin/sh

# Materialize the audited PCRE2 source subset used by WeiDU. The same script is
# called by GitHub Actions and local builds so dependency preparation cannot
# drift between release and developer environments.
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)

# This file deliberately uses syntax understood by both POSIX sh and GNU Make.
# shellcheck source=../pcre2/version
. "$repo_root/pcre2/version"

deps_dir="$repo_root/_deps"
source_dir="$deps_dir/pcre2-$PCRE2_VERSION"
stamp="$source_dir/.prepared"
upstream_dir=${PCRE2_UPSTREAM_DIR:-${1:-}}
temporary_upstream=
stage=

# Include every repository-controlled preparation input in the stamp. Merely
# matching the upstream commit is insufficient when the audited subset or the
# generated configuration changes without a PCRE2 version bump.
input_checksum=$(
  CDPATH= cd -- "$repo_root"
  cksum pcre2/version pcre2/sources.list pcre2/LICENCE.md \
    scripts/prepare_pcre2.sh |
    cksum | awk '{ print $1 ":" $2 }'
)
expected_stamp="$PCRE2_COMMIT:$input_checksum"

cleanup()
{
  if test -n "$stage" && test -d "$stage"; then
    rm -rf -- "$stage"
  fi
  if test -n "$temporary_upstream" && test -d "$temporary_upstream"; then
    rm -rf -- "$temporary_upstream"
  fi
}
trap cleanup EXIT HUP INT TERM

if test -f "$stamp" && test "$(sed -n '1p' "$stamp")" = "$expected_stamp"; then
  exit 0
fi

mkdir -p "$deps_dir"

if test -z "$upstream_dir"; then
  temporary_upstream=$(mktemp -d "$deps_dir/.pcre2-upstream.XXXXXX")
  upstream_dir=$temporary_upstream
  git -C "$upstream_dir" init --quiet
  git -C "$upstream_dir" remote add origin "$PCRE2_REPOSITORY"
  git -C "$upstream_dir" fetch --quiet --depth=1 origin "$PCRE2_COMMIT"
  git -C "$upstream_dir" checkout --quiet --detach FETCH_HEAD
fi

actual_commit=$(git -C "$upstream_dir" rev-parse HEAD^{commit})
if test "$actual_commit" != "$PCRE2_COMMIT"; then
  echo "PCRE2 checkout is $actual_commit; expected $PCRE2_COMMIT ($PCRE2_TAG)" >&2
  exit 1
fi

if test "$PCRE2_TAG" != "pcre2-$PCRE2_VERSION"; then
  echo "PCRE2 tag $PCRE2_TAG does not agree with version $PCRE2_VERSION" >&2
  exit 1
fi

# Compare committed blobs rather than checkout bytes: Git for Windows may
# materialize upstream Markdown with CRLF even though both repositories store
# the same LF-normalized licence object.
upstream_licence=$(git -C "$upstream_dir" rev-parse HEAD:LICENCE.md)
bundled_licence=$(git -C "$repo_root" rev-parse HEAD:pcre2/LICENCE.md)
if test "$upstream_licence" != "$bundled_licence"; then
  echo "pcre2/LICENCE.md does not match PCRE2 $PCRE2_TAG" >&2
  exit 1
fi

stage=$(mktemp -d "$deps_dir/.pcre2-$PCRE2_VERSION.XXXXXX")
while read -r upstream_path prepared_path extra; do
  case "$upstream_path" in
    ''|'#'*) continue ;;
  esac
  if test -n "${extra:-}"; then
    echo "Invalid entry in pcre2/sources.list: $upstream_path $prepared_path $extra" >&2
    exit 1
  fi
  case "$upstream_path:$prepared_path" in
    src/*:src/*) ;;
    *) echo "Unsafe entry in pcre2/sources.list: $upstream_path $prepared_path" >&2; exit 1 ;;
  esac
  if test ! -f "$upstream_dir/$upstream_path"; then
    echo "PCRE2 source is missing $upstream_path" >&2
    exit 1
  fi
  mkdir -p "$stage/$(dirname -- "$prepared_path")"
  cp "$upstream_dir/$upstream_path" "$stage/$prepared_path"
done < "$repo_root/pcre2/sources.list"

# PCRE2's generic configuration is intentionally changed in only these two
# places: WeiDU builds the 8-bit library and preserves Unicode/UTF support.
awk '
  /^\/\* #undef SUPPORT_PCRE2_8 \*\/$/ { print "#define SUPPORT_PCRE2_8 1"; next }
  /^\/\* #undef SUPPORT_UNICODE \*\/$/ { print "#define SUPPORT_UNICODE 1"; next }
  { print }
' "$stage/src/config.h" > "$stage/src/config.h.tmp"
mv "$stage/src/config.h.tmp" "$stage/src/config.h"

header_version=$(awk '
  /^#define PCRE2_MAJOR / { major = $3 }
  /^#define PCRE2_MINOR / { minor = $3 }
  END { print major "." minor }
' "$stage/src/pcre2.h")
if test "$header_version" != "$PCRE2_VERSION"; then
  echo "PCRE2 header is version $header_version; expected $PCRE2_VERSION" >&2
  exit 1
fi

if test "$(grep -c '^#define SUPPORT_PCRE2_8 1$' "$stage/src/config.h")" -ne 1 ||
   test "$(grep -c '^#define SUPPORT_UNICODE 1$' "$stage/src/config.h")" -ne 1; then
  echo "Failed to configure the PCRE2 8-bit Unicode build" >&2
  exit 1
fi

printf '%s\n%s\n' "$expected_stamp" "$PCRE2_TAG" > "$stage/.prepared"

# source_dir is derived from the fixed repository manifest and is constrained
# to _deps above, so replacement cannot target an arbitrary user path.
case "$source_dir" in
  "$deps_dir"/pcre2-*) ;;
  *) echo "Unsafe PCRE2 output directory: $source_dir" >&2; exit 1 ;;
esac
rm -rf -- "$source_dir"
mv "$stage" "$source_dir"
stage=
