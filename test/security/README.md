# Security regression tests

Sandbox launcher tests do not require a WeiDU build or Docker/Podman:

```bash
python test/security/test_weidu_sandbox.py
```

The `fixtures/danger` directory is a manual integration fixture for the
container sandbox. To use it, copy `fixtures/danger/setup-danger.tp2` to
the game root and copy the remaining fixture files as the game's
`danger/` folder, then run the launcher against that game with Docker or
Podman enabled. The fixture performs copy, large copy, move, delete, and
shell actions so the sandbox report should show changes only in the
temporary game copy. It also includes outside-game shell attempts with
stderr redirected to `/dev/null`; those should appear through the syscall
trace diagnostics rather than stdout/stderr parsing.
