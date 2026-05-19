#!/usr/bin/env python3
"""Unit tests for the sandbox launcher.

These tests intentionally avoid running Docker, Podman, or WeiDU. They verify
the pure Python parts of the wrapper: runtime selection, argument forwarding,
container command construction, snapshot diffs, and JSON report writing.
"""

import importlib.util
import json
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest import mock


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT = REPO_ROOT / "scripts" / "weidu-sandbox.py"

# The launcher filename contains a hyphen, so it cannot be imported with a
# regular `import scripts.weidu-sandbox`. Load it directly from its path instead.
spec = importlib.util.spec_from_file_location("weidu_sandbox", SCRIPT)
if spec is None or spec.loader is None:
    raise RuntimeError(f"Could not load sandbox launcher from {SCRIPT}")
weidu_sandbox = importlib.util.module_from_spec(spec)
spec.loader.exec_module(weidu_sandbox)


def fake_which_present(name: str) -> str:
    """Pretend every requested executable is available on PATH."""
    return f"/bin/{name}"


def fake_which_podman_only(name: str) -> str | None:
    """Pretend Docker is missing and Podman is available."""
    return None if name == "docker" else f"/bin/{name}"


class SandboxLauncherTests(unittest.TestCase):
    """Fast, dependency-free coverage for launcher behavior."""

    def test_runtime_auto_prefers_docker(self):
        """`auto` uses Docker first when both supported runtimes are present."""
        with mock.patch.object(shutil, "which", side_effect=fake_which_present):
            self.assertEqual(weidu_sandbox.find_runtime("auto"), "docker")

    def test_runtime_auto_falls_back_to_podman(self):
        """`auto` falls back to Podman when Docker is not available."""
        with mock.patch.object(shutil, "which", side_effect=fake_which_podman_only):
            self.assertEqual(weidu_sandbox.find_runtime("auto"), "podman")

    def test_runtime_missing_raises(self):
        """Missing container runtimes should produce a clear launcher error."""
        with mock.patch.object(shutil, "which", return_value=None):
            with self.assertRaises(RuntimeError):
                weidu_sandbox.find_runtime("auto")

    def test_args_after_separator_are_forwarded(self):
        """Everything after `--` belongs to WeiDU, not to the wrapper."""
        parsed = weidu_sandbox.parse_args(
            ["--game", "game", "--", "--nogame", "--force-install", "0", "setup.tp2"]
        )
        self.assertEqual(
            parsed.weidu_args,
            ["--nogame", "--force-install", "0", "setup.tp2"],
        )

    def test_packaged_build_context_is_preferred(self):
        """Release archives keep Docker build inputs under `sandbox-src`."""
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            packaged = root / "sandbox-src"
            packaged.mkdir()
            (packaged / "Dockerfile").write_text("FROM scratch\n", encoding="utf-8")
            (packaged / "Makefile").write_text("all:\n", encoding="utf-8")
            (packaged / "Depends").write_text("\n", encoding="utf-8")
            (packaged / "src").mkdir()

            self.assertEqual(weidu_sandbox.sandbox_build_context(root), packaged)

    def test_missing_build_context_raises(self):
        """A release missing sandbox sources should fail before invoking Docker."""
        with tempfile.TemporaryDirectory() as tmp:
            with self.assertRaises(RuntimeError):
                weidu_sandbox.sandbox_build_context(Path(tmp))

    def test_container_command_contains_sandbox_flags(self):
        """The generated command should include the hardening flags we rely on."""
        command = weidu_sandbox.build_container_command(
            "docker",
            "weidu-sandbox:local",
            Path("/tmp/game-copy"),
            Path("/tmp/diagnostics"),
            ["--nogame"],
        )
        self.assertIn("--network", command)
        self.assertIn("none", command)
        self.assertIn("--cap-drop", command)
        self.assertIn("ALL", command)
        self.assertIn("--read-only", command)
        self.assertIn("--mount", command)
        expected_mount = f"type=bind,src={Path('/tmp/game-copy')},target=/game"
        expected_diagnostics_mount = (
            f"type=bind,src={Path('/tmp/diagnostics')},target=/sandbox-diagnostics"
        )
        self.assertIn(expected_mount, command)
        self.assertIn(expected_diagnostics_mount, command)
        self.assertNotIn(f"{expected_mount},rw", command)
        self.assertIn("--entrypoint", command)
        self.assertIn("/usr/bin/strace", command)
        image_index = command.index("weidu-sandbox:local")
        self.assertEqual(
            command[image_index + 1 :],
            [
                "-f",
                "-qq",
                "-e",
                "trace=file",
                "-s",
                "4096",
                "-o",
                "/sandbox-diagnostics/strace.log",
                "/src/weidu",
                "--nogame",
            ],
        )

    def test_copy_snapshot_and_diff(self):
        """Snapshot diffs should report added, deleted, and modified files."""
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            src = root / "src"
            dst = root / "dst"
            src.mkdir()
            # Create one path for each diff class plus one unchanged control.
            (src / "same.txt").write_text("same\n", encoding="utf-8")
            (src / "modify.txt").write_text("old\n", encoding="utf-8")
            (src / "delete.txt").write_text("gone\n", encoding="utf-8")

            weidu_sandbox.copy_game_dir(src, dst)
            before = weidu_sandbox.snapshot_tree(dst)
            # Mutate only the temporary copy, mirroring how sandbox runs work.
            (dst / "modify.txt").write_text("new\n", encoding="utf-8")
            (dst / "delete.txt").unlink()
            (dst / "add.txt").write_text("added\n", encoding="utf-8")
            after = weidu_sandbox.snapshot_tree(dst)

            statuses = {
                (change["path"], change["status"])
                for change in weidu_sandbox.diff_snapshots(before, after)
            }
            self.assertEqual(
                statuses,
                {
                    ("modify.txt", "modified"),
                    ("delete.txt", "deleted"),
                    ("add.txt", "added"),
                },
            )

    def test_write_report_creates_json(self):
        """Report writing should create parent directories and valid JSON."""
        with tempfile.TemporaryDirectory() as tmp:
            report = Path(tmp) / "nested" / "report.json"
            weidu_sandbox.write_report(report, {"exit_code": 0, "changes": []})
            data = json.loads(report.read_text(encoding="utf-8"))
            self.assertEqual(data["exit_code"], 0)

    def test_format_timestamp_is_readable_utc(self):
        """Report timestamps should be stable, readable UTC strings."""
        self.assertEqual(
            weidu_sandbox.format_timestamp(0),
            "1970-01-01T00:00:00.000Z",
        )

    def test_diagnostics_parse_strace_outside_game_syscalls(self):
        """Strace should catch outside writes even when shell output is silent."""
        strace_log = "\n".join(
            [
                (
                    '123 openat(AT_FDCWD, "/sandbox-silent-escape.txt", '
                    "O_WRONLY|O_CREAT|O_TRUNC, 0666) = -1 EROFS "
                    "(Read-only file system)"
                ),
                (
                    '123 openat(AT_FDCWD, "../sandbox-silent-parent.txt", '
                    "O_WRONLY|O_CREAT|O_TRUNC, 0666) = -1 EROFS "
                    "(Read-only file system)"
                ),
                (
                    '123 openat(AT_FDCWD, "/game/override/inside.txt", '
                    "O_WRONLY|O_CREAT|O_TRUNC, 0666) = 4"
                ),
            ]
        )
        diagnostics = weidu_sandbox.build_diagnostics(strace_log)
        outside = diagnostics["outside_game_access"]

        self.assertTrue(outside["detected"])
        self.assertEqual(len(outside["syscall_events"]), 2)
        self.assertEqual(len(diagnostics["syscall_trace"]["interesting_events"]), 2)
        detected_paths = {
            path["path"]
            for event in outside["syscall_events"]
            for path in event["paths"]
        }
        self.assertEqual(
            detected_paths,
            {"/sandbox-silent-escape.txt", "../sandbox-silent-parent.txt"},
        )

    def test_diagnostics_parse_strace_tmpfs_syscalls(self):
        """Strace should classify /tmp writes as ephemeral, not host escapes."""
        strace_log = (
            '321 openat(AT_FDCWD, "/tmp/sandbox-temp.txt", '
            "O_WRONLY|O_CREAT|O_TRUNC, 0666) = 4"
        )
        diagnostics = weidu_sandbox.build_diagnostics(strace_log)

        self.assertFalse(diagnostics["outside_game_access"]["detected"])
        self.assertTrue(diagnostics["container_tmpfs_access"]["detected"])
        self.assertEqual(
            diagnostics["container_tmpfs_access"]["syscall_events"][0]["paths"][0]["path"],
            "/tmp/sandbox-temp.txt",
        )

    def test_diagnostics_resolve_relative_paths_after_chdir(self):
        """Relative writes should be classified using traced chdir state."""
        strace_log = "\n".join(
            [
                '7 chdir("/tmp") = 0',
                (
                    '7 openat(AT_FDCWD, "relative-temp.txt", '
                    "O_WRONLY|O_CREAT|O_TRUNC, 0666) = 4"
                ),
                '8 chdir("/") = 0',
                (
                    '8 openat(AT_FDCWD, "relative-root.txt", '
                    "O_WRONLY|O_CREAT|O_TRUNC, 0666) = -1 EROFS "
                    "(Read-only file system)"
                ),
            ]
        )
        diagnostics = weidu_sandbox.build_diagnostics(strace_log)

        self.assertTrue(diagnostics["container_tmpfs_access"]["detected"])
        self.assertEqual(
            diagnostics["container_tmpfs_access"]["syscall_events"][0]["paths"][0][
                "resolved_path"
            ],
            "/tmp/relative-temp.txt",
        )
        self.assertTrue(diagnostics["outside_game_access"]["detected"])
        self.assertEqual(
            diagnostics["outside_game_access"]["syscall_events"][0]["paths"][0][
                "resolved_path"
            ],
            "/relative-root.txt",
        )

    def test_diagnostics_keeps_game_relative_parent_paths_inside(self):
        """Parent traversal inside /game should not be reported as escape."""
        strace_log = "\n".join(
            [
                '1 chdir("/game/subdir") = 0',
                (
                    '1 openat(AT_FDCWD, "../inside-game.txt", '
                    "O_WRONLY|O_CREAT|O_TRUNC, 0666) = 4"
                ),
            ]
        )
        diagnostics = weidu_sandbox.build_diagnostics(strace_log)

        self.assertFalse(diagnostics["outside_game_access"]["detected"])
        self.assertFalse(diagnostics["container_tmpfs_access"]["detected"])
        self.assertEqual(diagnostics["syscall_trace"]["interesting_events"], [])

    def test_diagnostics_summary_includes_syscalls(self):
        """Console summary should include silent attempts found by strace."""
        diagnostics = weidu_sandbox.build_diagnostics(
            '1 mkdir("/sandbox-silent-dir", 0777) = -1 EROFS (Read-only file system)',
        )
        summary = weidu_sandbox.summarize_diagnostics(diagnostics)

        self.assertIn("syscall mkdir", summary)
        self.assertIn("/sandbox-silent-dir", summary)
        self.assertIn("EROFS", summary)

    def test_diagnostics_reports_missing_strace_log(self):
        """Missing strace output should be explicit in the report."""
        diagnostics = weidu_sandbox.build_diagnostics(None, "strace log missing")

        self.assertFalse(diagnostics["syscall_trace"]["available"])
        self.assertEqual(diagnostics["syscall_trace"]["error"], "strace log missing")
        self.assertFalse(diagnostics["outside_game_access"]["detected"])


if __name__ == "__main__":
    unittest.main()
