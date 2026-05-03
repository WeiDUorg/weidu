# Security regression tests

Run from this directory:

- Windows (PowerShell):

```pwsh
perl .\run_tests.pl
```

- macOS (zsh/bash):

```bash
perl ./run_tests.pl
```

- Linux (bash/sh):

```bash
perl ./run_tests.pl
```

Optional:
  - Set `WEIDU_BIN` to a specific executable path.

These tests cover:
  - `--dry-run` interception for copy operations
  - `--require-sha256` failure path when hash tools are unavailable
  - `--strict-path-risk` blocking for OS-specific high-risk paths
  - sensitive path warnings that should not block by default
