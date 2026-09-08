# Patch timing regression tests and benchmarks

These tests require Python 3 and native baseline/candidate WeiDU executables.
They generate their own resources and use `--nogame`. No installed game or
third-party mod is needed. Run from the candidate checkout:

```sh
python3 test/performance/run_stats_test.py
python3 test/performance/patch_timings.py \
  --baseline /path/to/baseline/weidu \
  --candidate /path/to/candidate/weidu \
  --output /path/to/new-results-directory
```

The Stats test also requires the build's OCaml compiler on PATH. `--build`
selects an alternative object directory. It links the actual Stats module and
checks nested success/failure, single callback execution, stack restoration,
and coarse/explicit timing retention in both modes.

The Python harness checks baseline, candidate default, and candidate with
`--debug-timings`. It compares complete resource bytes against independently
constructed expected outputs for ALTER_EFFECT, CLONE_EFFECT, and DELETE_EFFECT.
Fixtures cover SPL/ITM global and ability effects and CRE V1/V2 effect records,
including nonmatches and repeated alteration. It also verifies backup contents,
reinstallation, uninstallation, nested function scope restoration, explicit
PATCH_TIME output, and uncaught read errors. Five existing game-independent
TP2 regression files run with unsuccessful results converted into fatal errors.

Use `--skip-benchmark` for correctness alone. Benchmark defaults are configurable:
`--files 1000 --effects 10 --iterations 1000 --samples 10`. At most 999 effects
are allowed so ALTER_EFFECT's default match limit cannot silently leave part of
the workload unchanged. `--scenarios memory many copy` selects in-memory repeated
patches, directory-wide patches, and an unpatched COPY control.

Each scenario measures fresh installation and reinstallation separately. Each
phase has one warm-up pair and at least ten alternating baseline/candidate pairs.
Fixture creation, reinstall preparation, and output validation are outside the
timed process. Repeated runs use the host's normal filesystem cache; these are
not cold-storage throughput measurements. Use identical toolchains and flags for
both revisions and avoid competing jobs on the same machine.

`results.json` contains executable hashes, host metadata, output hashes, raw wall
times, and Unix child CPU/block counters where available. CPU/block counters are
omitted on Windows, rather than reported as zero. Speedup summaries use paired
log ratios with a conservative Student-t 95% interval for ten or more pairs.
The interval describes observed run variability, not all hardware or workloads.
Performance is reported rather than used as a flaky fixed-percentage test gate.
Inspect every scenario for improvement or unexplained regressions before merging.
Correctness failures always produce a nonzero exit status.

Logs remain below the new output directory. Large benchmark resource/backup
directories are removed after validation. The correctness fixtures are retained.
Record source revisions and compiler/runner versions alongside the results;
binary hashes alone do not establish source provenance. Run syscall tracing
separately, since tracing strongly perturbs execution time.
