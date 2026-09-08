#!/usr/bin/env python3
"""Compile Stats tests against an existing WeiDU native build, in a temp dir."""
import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--build", type=Path, default=Path("obj"))
args = parser.parse_args()
matches = list(args.build.resolve().rglob("stats.cmx"))
if len(matches) != 1:
    parser.error("Expected exactly one built stats.cmx below --build")
objects = matches[0].parent
with tempfile.TemporaryDirectory(prefix="weidu-stats-test-") as temporary:
    work = Path(temporary)
    source = Path(__file__).with_name("stats_test.ml").resolve()
    shutil.copyfile(source, work / source.name)
    modules = ("batList", "batteriesInit", "myhashtbl", "hashtblinit", "stats")
    command = ["ocamlopt", "-unsafe-string", "-I", str(objects), "unix.cmxa"]
    command += [str(objects / (module + ".cmx")) for module in modules]
    command += [source.name, "-o", "stats_test.exe"]
    subprocess.run(command, cwd=work, check=True)
    subprocess.run([str(work / "stats_test.exe")], cwd=work, check=True)
