#!/usr/bin/env python3
"""Run WeiDU in an OS-level sandbox against a temporary game copy.

The launcher deliberately keeps sandboxing outside the OCaml binary. It copies
the selected game directory to a host temp directory, mounts that copy plus a
private diagnostics directory into Docker or Podman, runs WeiDU there, and
reports the file changes seen in the copy. The original game directory is never
mounted into the container.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import os
import platform
import re
import shutil
import subprocess
import sys
import tempfile
import time
from collections.abc import Callable
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, cast


REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_IMAGE = "weidu-sandbox:local"
PACKAGED_BUILD_CONTEXT = "sandbox-src"
SANDBOX_DIAGNOSTICS_TARGET = "/sandbox-diagnostics"
STRACE_LOG_NAME = "strace.log"
DEFAULT_CONTAINER_CWD = "/game"
STRACE_LINE_RE = re.compile(
    r"^(?:(?P<pid>\d+)\s+)?(?P<syscall>[A-Za-z_][A-Za-z0-9_]*)"
    r"\((?P<args>.*)\)\s+=\s+(?P<result>.+)$"
)
STRACE_QUOTED_RE = re.compile(r'"((?:\\.|[^"\\])*)"')
MUTATING_SYSCALLS = {
    "chmod",
    "creat",
    "fchmodat",
    "fchmodat2",
    "link",
    "linkat",
    "mkdir",
    "mkdirat",
    "mknod",
    "mknodat",
    "rename",
    "renameat",
    "renameat2",
    "rmdir",
    "symlink",
    "symlinkat",
    "truncate",
    "unlink",
    "unlinkat",
    "utime",
    "utimensat",
    "utimes",
}
OPEN_SYSCALLS = {"open", "openat", "openat2"}
WRITE_OPEN_FLAGS = (
    "O_WRONLY",
    "O_RDWR",
    "O_CREAT",
    "O_TRUNC",
    "O_APPEND",
    "O_TMPFILE",
)


def normalize_weidu_args(args: list[str]) -> list[str]:
    """Strip argparse's remainder separator before forwarding args to WeiDU."""
    if args and args[0] == "--":
        return args[1:]
    return args


def parse_args(argv: list[str]) -> argparse.Namespace:
    """Parse launcher options and preserve all WeiDU arguments after '--'."""
    parser = argparse.ArgumentParser(
        description=(
            "Run WeiDU in Docker or Podman against a disposable copy of a game "
            "directory. The real game directory is never mounted into the container."
        )
    )
    parser.add_argument("--game", help="Path to the game directory to copy and test")
    parser.add_argument(
        "--runtime",
        choices=("auto", "docker", "podman"),
        default="auto",
        help="Container runtime to use (default: auto)",
    )
    parser.add_argument(
        "--image",
        default=DEFAULT_IMAGE,
        help=f"Sandbox image name (default: {DEFAULT_IMAGE})",
    )
    parser.add_argument(
        "--build-image",
        action="store_true",
        help="Build the sandbox image from this checkout or packaged release",
    )
    parser.add_argument(
        "--report",
        help="Write a JSON report with exit status, output, and changed paths",
    )
    parser.add_argument(
        "--keep-temp",
        action="store_true",
        help="Keep the temporary game copy for debugging",
    )
    parser.add_argument(
        "weidu_args",
        nargs=argparse.REMAINDER,
        help="Arguments passed to WeiDU after '--'",
    )
    parsed = parser.parse_args(argv)
    parsed.weidu_args = normalize_weidu_args(parsed.weidu_args)
    return parsed


def find_runtime(choice: str) -> str:
    """Resolve the requested container runtime to an executable name."""
    candidates = ["docker", "podman"] if choice == "auto" else [choice]
    for candidate in candidates:
        if shutil.which(candidate):
            return candidate
    wanted = "Docker or Podman" if choice == "auto" else choice
    raise RuntimeError(f"{wanted} was not found on PATH")


def is_source_build_context(path: Path) -> bool:
    """Return true when a directory has the files needed by the Dockerfile."""
    return (
        (path / "Dockerfile").is_file()
        and (path / "Makefile").is_file()
        and (path / "Depends").is_file()
        and (path / "src").is_dir()
    )


def sandbox_build_context(root: Path = REPO_ROOT) -> Path:
    """Find the Docker build context for source checkouts or release packages."""
    packaged = root / PACKAGED_BUILD_CONTEXT
    if is_source_build_context(packaged):
        return packaged
    if is_source_build_context(root):
        return root
    raise RuntimeError(
        "no sandbox build context found; expected a source checkout or a "
        f"release directory containing {PACKAGED_BUILD_CONTEXT}/"
    )


def build_image(runtime: str, image: str) -> None:
    """Build the local WeiDU sandbox image with the chosen runtime."""
    context = sandbox_build_context()
    subprocess.run(
        [runtime, "build", "-t", image, str(context)],
        cwd=context,
        check=True,
    )


def sha256_file(path: Path) -> str:
    """Return a content hash used for before/after file comparisons."""
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def entry_info(path: Path) -> dict[str, Any]:
    """Describe one filesystem entry in a stable, JSON-friendly shape."""
    st = path.lstat()
    if path.is_symlink():
        return {"kind": "symlink", "target": os.readlink(path)}
    if path.is_dir():
        return {"kind": "dir"}
    if path.is_file():
        return {
            "kind": "file",
            "size": st.st_size,
            "sha256": sha256_file(path),
        }
    return {"kind": "other", "mode": st.st_mode}


def snapshot_tree(root: Path) -> dict[str, dict[str, Any]]:
    """Snapshot every visible entry under root without following symlinks."""
    snapshot: dict[str, dict[str, Any]] = {}
    for dirpath, dirnames, filenames in os.walk(root, followlinks=False):
        current = Path(dirpath)
        for name in sorted(dirnames + filenames):
            path = current / name
            rel = path.relative_to(root).as_posix()
            snapshot[rel] = entry_info(path)
    return snapshot


def diff_snapshots(
    before: dict[str, dict[str, Any]],
    after: dict[str, dict[str, Any]],
) -> list[dict[str, Any]]:
    """Compare two snapshots and return added, deleted, or modified paths."""
    changes: list[dict[str, Any]] = []
    for path in sorted(set(before) | set(after)):
        if path not in before:
            changes.append({"path": path, "status": "added", "after": after[path]})
        elif path not in after:
            changes.append({"path": path, "status": "deleted", "before": before[path]})
        elif before[path] != after[path]:
            changes.append(
                {
                    "path": path,
                    "status": "modified",
                    "before": before[path],
                    "after": after[path],
                }
            )
    return changes


def copy_game_dir(src: Path, dst: Path) -> None:
    """Copy the game directory exactly enough for a disposable simulation run."""
    shutil.copytree(src, dst, symlinks=True, ignore_dangling_symlinks=True)


def container_user_args() -> list[str]:
    """Run as the host uid/gid on Unix so bind-mounted files stay writable."""
    getuid = cast(Callable[[], int] | None, getattr(os, "getuid", None))
    getgid = cast(Callable[[], int] | None, getattr(os, "getgid", None))
    if getuid is None or getgid is None:
        return []
    uid = getuid()
    gid = getgid()
    return ["--user", f"{uid}:{gid}"]


def build_container_command(
    runtime: str,
    image: str,
    game_copy: Path,
    diagnostics_dir: Path,
    weidu_args: list[str],
) -> list[str]:
    """Build the hardened Docker/Podman command line for the sandbox run."""
    # Docker's --mount syntax treats bind mounts as writable by default.
    # A bare ",rw" is accepted by some mount syntaxes but rejected here because
    # every --mount field must be key=value or a supported boolean option.
    game_mount = f"type=bind,src={game_copy},target=/game"
    diagnostics_mount = (
        f"type=bind,src={diagnostics_dir},target={SANDBOX_DIAGNOSTICS_TARGET}"
    )
    strace_log = f"{SANDBOX_DIAGNOSTICS_TARGET}/{STRACE_LOG_NAME}"
    return [
        runtime,  # Docker or Podman executable.
        "run",  # Start one container from the sandbox image.
        "--rm",  # Remove the container after WeiDU exits.
        "--pull",  # Control image-pull behavior explicitly.
        "never",  # Never contact a registry during sandbox runs.
        "--network",  # Configure container networking.
        "none",  # Disable network access for mod code.
        "--cap-drop",  # Remove Linux capabilities from the container.
        "ALL",  # Drop every optional capability.
        "--security-opt",  # Apply runtime security options.
        "no-new-privileges",  # Prevent privilege escalation through execve.
        "--read-only",  # Make the container root filesystem read-only.
        "--tmpfs",  # Provide a writable in-memory temp directory.
        "/tmp:rw,nosuid,nodev",  # Keep temp writable but block suid/dev nodes.
        "--memory",  # Limit container memory usage.
        "2g",  # Allow enough memory for large installs without being unbounded.
        "--cpus",  # Limit CPU scheduling.
        "2",  # Keep runaway scripts from consuming every host core.
        "--pids-limit",  # Limit process creation.
        "512",  # Prevent fork-heavy scripts from exhausting host resources.
        "--workdir",  # Set WeiDU's current directory.
        "/game",  # Run from the copied game directory.
        "--mount",  # Bind-mount one host path into the container.
        game_mount,  # Mount only the temporary game copy, never the real game.
        "--mount",  # Add a private temp diagnostics mount for strace output.
        diagnostics_mount,  # This is under the launcher temp dir, not the game.
        "--entrypoint",  # Override the image entrypoint for syscall tracing.
        "/usr/bin/strace",  # Trace WeiDU's file syscalls from inside the container.
        *container_user_args(),  # Preserve writable bind mounts on Unix hosts.
        image,  # Sandbox image containing the WeiDU binary.
        "-f",  # Follow child processes and shell commands spawned by WeiDU.
        "-qq",  # Keep the trace log focused on syscall lines.
        "-e",  # Restrict tracing to filesystem-related syscalls.
        "trace=file",  # Capture open, rename, unlink, mkdir, chmod, etc.
        "-s",  # Avoid truncating path strings too aggressively.
        "4096",  # Long enough for typical mod/game paths.
        "-o",  # Write trace output to the private diagnostics mount.
        strace_log,  # Host-readable after the container exits.
        "/src/weidu",  # The actual WeiDU binary inside the sandbox image.
        *weidu_args,  # Forward the user's WeiDU arguments unchanged.
    ]


def run_container(command: list[str]) -> subprocess.CompletedProcess[str]:
    """Execute the container and capture WeiDU output for the final report."""
    return subprocess.run(command, text=True, capture_output=True)


def read_strace_log(diagnostics_dir: Path) -> tuple[str | None, str | None]:
    """Read the syscall trace captured from the sandbox container."""
    path = diagnostics_dir / STRACE_LOG_NAME
    if not path.exists():
        return None, f"strace log was not produced at {path}"
    try:
        return path.read_text(encoding="utf-8", errors="replace"), None
    except OSError as exc:
        return None, f"could not read strace log at {path}: {exc}"


def write_report(path: Path, report: dict[str, Any]) -> None:
    """Write the machine-readable sandbox report."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def format_timestamp(epoch_seconds: float) -> str:
    """Format a Unix timestamp as a readable UTC ISO-8601 string."""
    return (
        datetime.fromtimestamp(epoch_seconds, tz=timezone.utc)
        .isoformat(timespec="milliseconds")
        .replace("+00:00", "Z")
    )


def summarize_changes(changes: list[dict[str, Any]]) -> str:
    """Create a compact console summary; the JSON report keeps full details."""
    if not changes:
        return "No file changes were observed in the temporary game copy."
    shown = changes[:20]
    lines = [f"{len(changes)} changed path(s) in the temporary game copy:"]
    lines.extend(f"  {change['status']}: {change['path']}" for change in shown)
    if len(changes) > len(shown):
        lines.append(f"  ... {len(changes) - len(shown)} more")
    return "\n".join(lines)


def normalize_container_path(path: str, cwd: str = DEFAULT_CONTAINER_CWD) -> str:
    """Resolve a container path lexically without touching the host filesystem."""
    normalized = path.replace("\\", "/")
    if not normalized.startswith("/"):
        normalized = f"{cwd.rstrip('/')}/{normalized}"
    parts: list[str] = []
    for part in normalized.split("/"):
        if part in ("", "."):
            continue
        if part == "..":
            if parts:
                parts.pop()
            continue
        parts.append(part)
    return "/" + "/".join(parts)


def classify_container_path(
    raw_path: str,
    cwd: str = DEFAULT_CONTAINER_CWD,
) -> dict[str, str]:
    """Classify a traced path relative to the sandbox mount layout."""
    path = raw_path
    normalized = path.replace("\\", "/")
    if re.match(r"^[A-Za-z]:/", normalized):
        return {
            "path": path,
            "resolved_path": normalized,
            "relation": "host_absolute_syntax",
            "meaning": "Windows-style absolute path requested from inside Linux sandbox",
        }
    resolved = normalize_container_path(normalized, cwd)
    if resolved == "/game" or resolved.startswith("/game/"):
        return {
            "path": path,
            "resolved_path": resolved,
            "relation": "inside_game_copy",
            "meaning": "temporary game copy mounted at /game",
        }
    if resolved == "/tmp" or resolved.startswith("/tmp/"):
        return {
            "path": path,
            "resolved_path": resolved,
            "relation": "container_tmpfs",
            "meaning": "ephemeral container /tmp, discarded after the run",
        }
    return {
        "path": path,
        "resolved_path": resolved,
        "relation": "outside_game",
        "meaning": "container path outside /game",
    }


def decode_strace_string(raw: str) -> str:
    """Decode one quoted string from strace output."""
    try:
        decoded = ast.literal_eval(f'"{raw}"')
        return decoded if isinstance(decoded, str) else raw
    except (SyntaxError, ValueError):
        return raw


def extract_strace_paths(
    args: str,
    cwd: str = DEFAULT_CONTAINER_CWD,
) -> list[dict[str, str]]:
    """Extract quoted path arguments from one strace syscall argument list."""
    paths: list[dict[str, str]] = []
    seen: set[str] = set()
    for raw in STRACE_QUOTED_RE.findall(args):
        path = decode_strace_string(raw)
        if path in seen:
            continue
        seen.add(path)
        paths.append(classify_container_path(path, cwd))
    return paths


def syscall_can_mutate(syscall: str, args: str) -> bool:
    """Return true for file syscalls that can mutate filesystem state."""
    if syscall in MUTATING_SYSCALLS:
        return True
    if syscall in OPEN_SYSCALLS:
        return any(flag in args for flag in WRITE_OPEN_FLAGS)
    return False


def parse_strace_result(result: str) -> dict[str, Any]:
    """Parse success/failure details from the right side of a strace line."""
    failure = re.match(r"-1\s+(?P<errno>[A-Z0-9]+)\s+\((?P<message>[^)]*)\)", result)
    if failure:
        return {
            "success": False,
            "errno": failure.group("errno"),
            "message": failure.group("message"),
            "raw": result,
        }
    return {"success": True, "errno": None, "message": None, "raw": result}


def parse_strace_log(text: str) -> list[dict[str, Any]]:
    """Parse mutating filesystem syscalls from a strace log."""
    events: list[dict[str, Any]] = []
    cwd_by_pid: dict[str, str] = {"main": DEFAULT_CONTAINER_CWD}
    for line_number, line in enumerate(text.splitlines(), start=1):
        match = STRACE_LINE_RE.match(line)
        if not match:
            continue
        pid = match.group("pid") or "main"
        syscall = match.group("syscall")
        args = match.group("args")
        result = parse_strace_result(match.group("result"))
        cwd = cwd_by_pid.get(pid, DEFAULT_CONTAINER_CWD)
        if syscall == "chdir":
            paths = extract_strace_paths(args, cwd)
            if result["success"] and paths:
                cwd_by_pid[pid] = paths[0]["resolved_path"]
            continue
        if not syscall_can_mutate(syscall, args):
            continue
        paths = extract_strace_paths(args, cwd)
        relevant_paths = [
            path
            for path in paths
            if path["relation"]
            in ("outside_game", "host_absolute_syntax", "container_tmpfs")
        ]
        if not relevant_paths:
            continue
        events.append(
            {
                "line_number": line_number,
                "line": line,
                "syscall": syscall,
                "result": result,
                "paths": relevant_paths,
            }
        )
    return events


def build_diagnostics(
    strace_text: str | None = None,
    strace_error: str | None = None,
) -> dict[str, Any]:
    """Build report diagnostics for sandbox escape attempts."""
    syscall_events = parse_strace_log(strace_text) if strace_text is not None else []
    outside_syscalls = [
        event
        for event in syscall_events
        if any(
            path["relation"] in ("outside_game", "host_absolute_syntax")
            for path in event["paths"]
        )
    ]
    tmpfs_syscalls = [
        event
        for event in syscall_events
        if any(path["relation"] == "container_tmpfs" for path in event["paths"])
    ]
    return {
        "host_mutation_model": {
            "real_game_mounted": False,
            "writable_host_mounts": [
                "/game temporary copy",
                f"{SANDBOX_DIAGNOSTICS_TARGET} temporary diagnostics",
            ],
            "container_root": "read-only",
            "network": "none",
            "ephemeral_tmpfs": ["/tmp"],
        },
        "syscall_trace": {
            "enabled": True,
            "available": strace_text is not None,
            "error": strace_error,
            "interesting_events": syscall_events,
            "note": (
                "Syscall trace is collected with strace -f -e trace=file inside "
                "the container and parsed for outside-game and tmpfs mutating "
                "filesystem calls."
            ),
        },
        "outside_game_access": {
            "detected": bool(outside_syscalls),
            "syscall_events": outside_syscalls,
            "note": (
                "Events come from strace and can reveal attempts whose shell "
                "errors were suppressed."
            ),
        },
        "container_tmpfs_access": {
            "detected": bool(tmpfs_syscalls),
            "syscall_events": tmpfs_syscalls,
            "note": "/tmp is writable inside the container but is discarded after the run.",
        },
    }


def summarize_diagnostics(diagnostics: dict[str, Any]) -> str:
    """Create a short human-readable summary for important diagnostics."""
    outside = diagnostics["outside_game_access"]
    if not outside["detected"]:
        return "No outside-game path attempts were detected in the syscall trace."
    syscall_events = outside["syscall_events"]
    lines = [f"{len(syscall_events)} outside-game syscall diagnostic(s) found:"]
    for event in syscall_events[:10]:
        paths = ", ".join(path["path"] for path in event["paths"])
        result = event["result"]
        if result["success"]:
            detail = "success"
        else:
            detail = f"{result['errno']} ({result['message']})"
        lines.append(f"  syscall {event['syscall']}:{event['line_number']}: {detail}: {paths}")
    shown = len(lines) - 1
    if len(syscall_events) > shown:
        lines.append(f"  ... {len(syscall_events) - shown} more")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    """CLI entry point."""
    args = parse_args(sys.argv[1:] if argv is None else argv)
    try:
        runtime = find_runtime(args.runtime)
        if args.build_image:
            build_image(runtime, args.image)
            if not args.game:
                print(f"Built sandbox image {args.image} with {runtime}.")
                return 0

        if not args.game:
            print("ERROR: --game is required unless --build-image is used alone.", file=sys.stderr)
            return 2

        game = Path(args.game).resolve()
        if not game.is_dir():
            print(f"ERROR: game directory does not exist: {game}", file=sys.stderr)
            return 2

        temp_root = Path(tempfile.mkdtemp(prefix="weidu-sandbox-"))
        game_copy = temp_root / "game"
        diagnostics_dir = temp_root / "diagnostics"
        start_time = time.time()
        try:
            # The real game directory is intentionally copied, not bind-mounted.
            # A report-only sandbox must never mutate the original installation.
            copy_game_dir(game, game_copy)

            # File changes are detected from snapshots of the temporary copy.
            # This avoids relying on WeiDU internals or container-specific logs.
            diagnostics_dir.mkdir()
            before = snapshot_tree(game_copy)
            command = build_container_command(
                runtime,
                args.image,
                game_copy,
                diagnostics_dir,
                args.weidu_args,
            )
            completed = run_container(command)
            after = snapshot_tree(game_copy)
            changes = diff_snapshots(before, after)
            strace_text, strace_error = read_strace_log(diagnostics_dir)
            diagnostics = build_diagnostics(
                strace_text,
                strace_error,
            )

            # Preserve WeiDU's normal stdout/stderr behavior for the user while
            # also keeping the same data in the optional JSON report.
            if completed.stdout:
                sys.stdout.write(completed.stdout)
            if completed.stderr:
                sys.stderr.write(completed.stderr)

            finished_time = time.time()
            report: dict[str, Any] = {
                "runtime": runtime,
                "image": args.image,
                "host": {
                    "platform": platform.platform(),
                    "python": platform.python_version(),
                },
                "game": str(game),
                "temporary_game": str(game_copy),
                "temporary_diagnostics": str(diagnostics_dir),
                "temporary_game_kept": args.keep_temp,
                "weidu_args": args.weidu_args,
                "container_command": command,
                "exit_code": completed.returncode,
                "started_at": format_timestamp(start_time),
                "finished_at": format_timestamp(finished_time),
                "duration_seconds": round(finished_time - start_time, 3),
                "started_at_epoch": start_time,
                "finished_at_epoch": finished_time,
                "stdout": completed.stdout,
                "stderr": completed.stderr,
                "changes": changes,
                "diagnostics": diagnostics,
            }
            if args.report:
                report_path = Path(args.report).resolve()
                write_report(report_path, report)
                print(f"Sandbox report written to {report_path}")
            print(summarize_changes(changes))
            print(summarize_diagnostics(diagnostics))
            return completed.returncode
        finally:
            if args.keep_temp:
                print(f"Temporary game copy kept at {game_copy}")
            else:
                # Temp cleanup is best-effort. A failed cleanup should not hide
                # the actual WeiDU/container result from the user.
                shutil.rmtree(temp_root, ignore_errors=True)
    except subprocess.CalledProcessError as exc:
        print(f"ERROR: command failed with exit code {exc.returncode}: {exc.cmd}", file=sys.stderr)
        return exc.returncode or 1
    except RuntimeError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
