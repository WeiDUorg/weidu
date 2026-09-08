#!/usr/bin/env python3
"""Game-independent correctness checks and paired WeiDU patch benchmarks.

Uses only the Python standard library. All generated game data, installations,
and logs live under a new --output directory, never in a real game directory.
"""

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import re
import shutil
import statistics
import struct
import subprocess
import time

try:
    import resource
except ImportError:
    resource = None


DETAILS = ("READ_*", "process_patch2", "eval_pe", "function overhead")
# Infinity Engine V1 SPL/ITM/CRE layouts, including CRE's V2 effect records.
FORMATS = {
    "spl": (b"SPL V1  ", 0x72, 0x28, 0),
    "itm": (b"ITM V1  ", 0x72, 0x38, 0),
    "cre": (b"CRE V1.0", 0x2D4, 0, 0),
    "cre-v2": (b"CRE V1.0", 0x2D4, 0, 1),
}
MOD_HEADER = 'BACKUP ~backup~\nAUTHOR ~WeiDU regression tests~\n'
MOD_HEADER += 'VERSION ~synthetic~\nBEGIN ~Patch timing tests~\n'


def digest(data):
    return hashlib.sha256(data).hexdigest()


def effect(opcode, parameter, extended):
    result = bytearray(0x108 if extended else 0x30)
    if extended:
        result[:8] = b"EFF V2.0"
    struct.pack_into("<H", result, 0x08 if extended else 0, opcode)
    struct.pack_into("<I", result, 0x14 if extended else 4, parameter)
    result[0x24 if extended else 0x12] = 100
    offset = 0x28 if extended else 0x14
    result[offset:offset + 8] = b"testres\0"
    return bytes(result)


def render(kind, globals_, ability=()):
    """Build expected resources from record lists, independently of TP2 patches."""
    signature, header_size, ability_size, extended = FORMATS[kind]
    has_ability = bool(ability_size and ability)
    effect_offset = header_size + (ability_size if has_ability else 0)
    header = bytearray(effect_offset)
    header[:8] = signature
    if ability_size:
        struct.pack_into("<I", header, 0x64, header_size)
        struct.pack_into("<H", header, 0x68, int(has_ability))
        struct.pack_into("<I", header, 0x6A, effect_offset)
        struct.pack_into("<H", header, 0x70, len(globals_))
        if has_ability:
            header[header_size] = 1
            struct.pack_into("<HH", header, header_size + 0x1E,
                             len(ability), len(globals_))
    else:
        header[0x33] = extended
        struct.pack_into("<II", header, 0x2C4, effect_offset, len(globals_))
    return bytes(header) + b"".join(effect(o, p, extended)
                                    for o, p in [*globals_, *ability])


def write_mod(directory, body):
    (directory / "test.tp2").write_text(MOD_HEADER + body, encoding="utf-8")


def invoke(binary, directory, label, detail=False, uninstall=False,
           tp2="test.tp2", expect_failure=False):
    log = directory / (label + ".debug")
    command = [str(binary), tp2, "--nogame", "--noautoupdate",
               "--no-exit-pause", "--log", str(log)]
    if detail:
        command.append("--debug-timings")
    command += ["--force-uninstall-list" if uninstall else
                "--force-install-list", "0"]
    before = resource.getrusage(resource.RUSAGE_CHILDREN) if resource else None
    start = time.perf_counter()
    process = subprocess.run(command, cwd=directory, stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT, timeout=300)
    elapsed = time.perf_counter() - start
    after = resource.getrusage(resource.RUSAGE_CHILDREN) if resource else None
    (directory / (label + ".stdout")).write_bytes(process.stdout)
    text = process.stdout.decode("utf-8", errors="replace")
    if (process.returncode != 0) != expect_failure:
        raise AssertionError(f"{label}: exit {process.returncode}\n{text}")
    if not expect_failure and not uninstall:
        if "SUCCESSFULLY INSTALLED" not in text or "SUPPRESSED ERROR" in text:
            raise AssertionError(f"{label}: installation did not pass\n{text}")
    result = {"wall_s": elapsed, "exit": process.returncode}
    if resource:
        result.update(user_s=after.ru_utime - before.ru_utime,
                      system_s=after.ru_stime - before.ru_stime,
                      input_blocks=after.ru_inblock - before.ru_inblock,
                      output_blocks=after.ru_oublock - before.ru_oublock)
    return result, log.read_text(encoding="utf-8", errors="replace")


def check_timings(log, detail):
    table = log.rsplit("WeiDU Timings", 1)[-1]
    if "WeiDU Timings" not in log:
        raise AssertionError("Missing timing summary")
    for name in DETAILS:
        found = re.search(r"^" + re.escape(name) + r"\s+\d", table, re.M)
        if bool(found) != detail:
            raise AssertionError(f"Unexpected timing presence: {name}: {detail}")
    for name in ("COPY", "TOTAL"):
        if not re.search(r"^" + name + r"\s+\d", table, re.M):
            raise AssertionError(f"Missing coarse timing: {name}")
    if "Mod Timings" not in log or "fixture-time" not in log:
        raise AssertionError("Explicit mod timing was lost")


def verify_outputs(directory, expected):
    actual = {p.name: p.read_bytes() for p in (directory / "output").iterdir()}
    if actual != expected:
        different = sorted(k for k in actual.keys() | expected.keys()
                           if actual.get(k) != expected.get(k))
        raise AssertionError(f"Output mismatch in {directory}: {different[:10]}")
    return {name: digest(data) for name, data in sorted(actual.items())}


def correctness(binary, directory, detail, baseline=False):
    directory.mkdir(parents=True)
    (directory / "input").mkdir()
    (directory / "output").mkdir()
    original = [(12, 7), (13, 8)]
    expected, originals = {}, {}
    body = '''DEFINE_PATCH_FUNCTION nested BEGIN
      LPF ALTER_EFFECT INT_VAR match_opcode=12 parameter1=42 silent=1 END
    END
    DEFINE_PATCH_FUNCTION failing BEGIN
      SET scope_witness = 999
      PATCH_FAIL ~expected nested failure~
    END
    '''
    operations = {
        "alter": ("LPF nested END\nLPF nested END", [(12, 42), (13, 8)]),
        "clone": ("LPF CLONE_EFFECT INT_VAR match_opcode=12 opcode=14 "
                  "parameter1=42 silent=1 STR_VAR insert=below END",
                  [(12, 7), (14, 42), (13, 8)]),
        "delete": ("LPF DELETE_EFFECT INT_VAR match_opcode=12 END", [(13, 8)]),
        "no-match": ("LPF ALTER_EFFECT INT_VAR match_opcode=65534 "
                     "parameter1=42 silent=1 END\n"
                     "LPF CLONE_EFFECT INT_VAR match_opcode=65534 silent=1 END\n"
                     "LPF DELETE_EFFECT INT_VAR match_opcode=65534 END", original),
    }
    for kind in FORMATS:
        ability = original if FORMATS[kind][2] else []
        for operation, (patch, records) in operations.items():
            filename = f"{kind}-{operation}.{kind.split('-')[0]}"
            data = render(kind, original, ability)
            (directory / "input" / filename).write_bytes(data)
            # Pre-existing destinations exercise real backups and restoration.
            (directory / "output" / filename).write_bytes(data)
            originals[filename] = data
            expected[filename] = render(kind, records, records if ability else [])
            body += f'COPY ~input/{filename}~ ~output/{filename}~\n'
            body += '  PATCH_TIME ~fixture-time~ BEGIN\n' + patch + '\nEND\n'
    body += '''COPY ~input/spl-alter.spl~ ~output/new.spl~
      SET scope_witness = 17
      SET caught = 0
      PATCH_TRY
        LPF failing END
      WITH
        DEFAULT
          SET caught = 1
      END
      PATCH_IF scope_witness != 17 OR caught != 1 BEGIN
        PATCH_FAIL ~Function failure did not restore scope~
      END
      LPF nested END
    '''
    expected["new.spl"] = render("spl", [(12, 42), (13, 8)], [(12, 42), (13, 8)])
    write_mod(directory, body)
    _, log = invoke(binary, directory, "install", detail)
    check_timings(log, baseline or detail)
    hashes = verify_outputs(directory, expected)
    backups = [p.read_bytes() for p in (directory / "backup").rglob("*")
               if p.is_file()]
    if not all(data in backups for data in originals.values()):
        raise AssertionError("Original resource backup is missing")
    invoke(binary, directory, "reinstall", detail)
    verify_outputs(directory, expected)
    invoke(binary, directory, "uninstall", detail, uninstall=True)
    verify_outputs(directory, originals)

    # An uncaught bounds error must retain its exit status and roll back.
    (directory / "short.bin").write_bytes(b"x")
    write_mod(directory, 'COPY ~short.bin~ ~output/failed.bin~\nREAD_LONG 0 value\n')
    failure, log = invoke(binary, directory, "failure", detail, expect_failure=True)
    if "read out of bounds" not in log or (directory / "output/failed.bin").exists():
        raise AssertionError("Bounds failure or rollback changed")
    return {"hashes": hashes, "failure_exit": failure["exit"]}


def regressions(binary, directory, detail):
    source = Path(__file__).resolve().parents[1] / "tp2/tp2_regression_tests"
    target = directory / "tp2_regression_tests"
    shutil.copytree(source, target)
    # These existing tests require no installed game or game-specific IDS.
    names = ("fun_eval", "array_tests", "test_sprintf", "variable_is_star", "read_2da")
    body = MOD_HEADER
    for name in names:
        body += f'INCLUDE ~%MOD_FOLDER%/lib/tests/{name}.tpa~\n'
        body += 'LAF run RET success message END\n'
        body += 'ACTION_IF !success BEGIN FAIL ~%message%~ END\n'
    (target / "tp2_regression_tests.tp2").write_text(body, encoding="utf-8")
    invoke(binary, directory, "regressions", detail,
           tp2="tp2_regression_tests/tp2_regression_tests.tp2")
    return list(names)


def prepare_benchmark(directory, scenario, args):
    directory.mkdir(parents=True)
    (directory / "input").mkdir()
    (directory / "output").mkdir()
    count = args.files if scenario != "memory" else 1
    records = [(12, 7)] * args.effects
    source = render("spl", records)
    expected_data = source if scenario == "copy" else render("spl", [(12, 42)] * args.effects)
    expected = {}
    for i in range(count):
        name = f"p{i:07d}.spl"
        (directory / "input" / name).write_bytes(source)
        expected[name] = expected_data
    patch = ""
    if scenario != "copy":
        repeats = args.iterations if scenario == "memory" else 1
        patch = f'''FOR (iteration = 0; iteration < {repeats}; ++iteration) BEGIN
          LPF ALTER_EFFECT INT_VAR match_opcode=12 parameter1=42 silent=1 END
        END
        '''
    write_mod(directory, 'COPY ~input~ ~output~\n' + patch)
    return expected


def summarize(samples):
    """Paired log ratios with a Student-t 95% interval (10+ samples)."""
    ratios = [math.log(pair["baseline"]["wall_s"] / pair["candidate"]["wall_s"])
              for pair in samples]
    # Conservative t bound for >=10 pairs (df >=9), avoiding a scipy dependency.
    radius = 2.263 * statistics.stdev(ratios) / math.sqrt(len(ratios))
    mean = statistics.mean(ratios)
    return {"geometric_speedup": math.exp(mean),
            "speedup_95pct_interval": [math.exp(mean - radius), math.exp(mean + radius)],
            "baseline_median_s": statistics.median(p["baseline"]["wall_s"] for p in samples),
            "candidate_median_s": statistics.median(p["candidate"]["wall_s"] for p in samples)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", type=Path, required=True)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--samples", type=int, default=10)
    parser.add_argument("--files", type=int, default=1000)
    parser.add_argument("--effects", type=int, default=10)
    parser.add_argument("--iterations", type=int, default=1000)
    parser.add_argument("--skip-benchmark", action="store_true")
    parser.add_argument("--scenarios", nargs="+", choices=("memory", "many", "copy"),
                        default=("memory", "many", "copy"))
    args = parser.parse_args()
    if min(args.files, args.effects, args.iterations) < 1 or args.samples < 10:
        parser.error("Use positive workload sizes and at least ten measured pairs")
    if args.effects > 999 or args.files > 10000000:
        parser.error("Keep effects within the function's default match limit and resrefs within eight characters")
    args.baseline = args.baseline.resolve(strict=True)
    args.candidate = args.candidate.resolve(strict=True)
    args.output = args.output.resolve()
    args.output.mkdir(parents=True, exist_ok=False)
    report = {"platform": platform.platform(), "machine": platform.machine(),
              "python": platform.python_version(), "cpu_count": os.cpu_count(),
              "configuration": {k: str(v) if isinstance(v, Path) else v
                                for k, v in vars(args).items()},
              "binary_sha256": {"baseline": digest(args.baseline.read_bytes()),
                                "candidate": digest(args.candidate.read_bytes())},
              "correctness": {}, "benchmarks": {}}

    def save():
        (args.output / "results.json").write_text(json.dumps(report, indent=2) + "\n")

    for label, binary, detail in (("baseline", args.baseline, False),
                                   ("candidate", args.candidate, False),
                                   ("candidate-detail", args.candidate, True)):
        report["correctness"][label] = correctness(binary, args.output / label, detail,
                                                   baseline=label == "baseline")
        regressions(binary, args.output / (label + "-regressions"), detail)
        save()
        print(f"{label}: correctness and existing regressions passed", flush=True)
    if not (report["correctness"]["baseline"] == report["correctness"]["candidate"] ==
            report["correctness"]["candidate-detail"]):
        raise AssertionError("Baseline/candidate correctness results differ")

    if not args.skip_benchmark:
        for scenario in args.scenarios:
            report["benchmarks"][scenario] = {}
            for phase in ("install", "reinstall"):
                pairs = []
                for sample in range(-1, args.samples):
                    pair = {}
                    order = ("baseline", "candidate") if sample % 2 == 0 else ("candidate", "baseline")
                    for label in order:
                        directory = args.output / "work" / f"{scenario}-{phase}-{sample}-{label}"
                        expected = prepare_benchmark(directory, scenario, args)
                        binary = getattr(args, label)
                        if phase == "reinstall":
                            invoke(binary, directory, "prepare")
                        pair[label], _ = invoke(binary, directory, "measured")
                        verify_outputs(directory, expected)
                        # Keep logs while avoiding tens of thousands of artifacts.
                        shutil.rmtree(directory / "input")
                        shutil.rmtree(directory / "output")
                        shutil.rmtree(directory / "backup")
                    if sample >= 0:
                        pairs.append(pair)
                    report["benchmarks"][scenario][phase] = {"samples": pairs}
                    save()
                summary = summarize(pairs)
                report["benchmarks"][scenario][phase]["summary"] = summary
                save()
                print(f"{scenario}/{phase}: {json.dumps(summary)}", flush=True)
    print(f"Results: {args.output / 'results.json'}", flush=True)


if __name__ == "__main__":
    main()
