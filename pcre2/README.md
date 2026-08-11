# Bundled PCRE2

WeiDU statically links a deliberately limited 8-bit PCRE2 build. Upstream
source is not copied into this repository. The build materializes a verified
subset under `_deps/` from the release and commit recorded in [`version`](version).

GitHub Actions performs this through the local `prepare-pcre2` composite action
before each platform build. A local `make` performs the same preparation on
first use; `make pcre2-source` may be run explicitly. Set `PCRE2_UPSTREAM_DIR`
to an existing PCRE2 checkout to avoid the fetch.

[`sources.list`](sources.list) is the audited transitive source closure used by
WeiDU's binding. It excludes unrelated PCRE2 APIs such as DFA matching, JIT,
pattern conversion, serialization, substitution, and substring helpers. The
preparation script also enables only the 8-bit and Unicode configurations.

To update PCRE2:

1. Update the version, tag, and exact commit in [`version`](version), and any
   user-facing version reference in the language documentation.
2. Compare the binding's referenced symbols with the new release and update
   [`sources.list`](sources.list) if the transitive closure changed; the
   Makefile derives its C module list from this file.
3. Update [`LICENCE.md`](LICENCE.md) if upstream changed it.
4. Run all platform builds and the PCRE2 regexp regression suite.

The source commit is checked before any file is copied, and the bundled licence
is checked byte-for-byte against that commit. A version update therefore cannot
silently use a different upstream tree.
