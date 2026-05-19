# Building WeiDU

## Requirements and build environment

The version of OCaml needs to be greater than or equal to 4.04 and
configured without forced safe strings.

### The Windows Unicode dilemma

If WeiDU is built with OCaml greater than or equal to 4.06 on Windows
and the environment variable `WINDOWS_UNICODE_MODE` is not set to
`ansi` when configuring the OCaml build, the resulting WeiDU will be
incompatible with mods that rely on legacy behaviour, notably the
mod Infinity Animations.

However, if WeiDU on Windows is built without Unicode-support, mods
that print Unicode characters to the terminal will be unable to do
so. This unsurprisingly include many mods that target languages that
use non-Latin alphabets.

Refer to the relevant section of the [OCaml
documentation](https://github.com/ocaml/ocaml/blob/trunk/README.win32.adoc#unicode-support)
for more information.

### GNU/Linux

- OCaml with native compilers. You either need to use
  [opam](https://opam.ocaml.org/) or compile OCaml from source, as
  WeiDU requires unsafe strings. With opam, you can obtain a suitable
  version by creating a switch:
  `opam switch create 4.11.2+default-unsafe-string`

- A basic GCC tool chain with make. This might come pre-installed.

- Perl, which is normally installed by default.

- Optionally, also git, hevea, texlive, zip and upx. HeVeA and TexLive
  are only needed to build the documentation and zip and upx are used
  in making the distributable archives (upx is optional).

- Elkhound (*vide infra*). Place the executable on you path and allow
  Elkhound to be executed as a program, for example, by using the
  terminal command `chmod +x path/to/elkhound`

### Windows

*N.B.* These instructions may not be step-for-step accurate, as no one
who develops WeiDU do so from Windows systems, so there is no current
first-hand experience.

- Native OCaml without forced safe strings, typically obtained through
  [opam](https://opam.ocaml.org/):
  `opam switch create --packages=ocaml-option-default-unsafe-string ocaml-variants.4.14.2+options`

- A Cygwin-based *nix tool chain, particularly `binutils` and `make`
  from the `Devel` group. Perl is also required but is typically
  installed by default. Optionally also `git` (Devel), `openssh`
  (Net), `zip` (Archive) and `upx` (Utils).

- Cygwin-hosted MinGW-GCC, called `mingw-gcc-core`, or some such.

- Elkhound (*vide infra*). For less configuration, place the elkhound
  binary in Cygwin's `/bin` directory, or equivalent. The build
  process does not like paths with spaces.

### MacOS

- A basic GCC tool chain with make.

- Install Perl (using MacPorts or HomeBrew, for example).

- OCaml with native compilers. You either need to use
  [opam](https://opam.ocaml.org/) or compile OCaml from source, as
  WeiDU requires unsafe strings. With opam, you can obtain a suitable
  version by creating a switch:
  `opam switch create 4.11.2+default-unsafe-string`

- Optionally, install UPX. UPX is used to compress the compiled
  programs, but is not available for all platforms.

- Obtain Elkhound (*vide infra*) and place the executable on your
  path. Allow Elkhound to be executed as a program, for example, by
  using the terminal command `chmod +x path/to/elkhound`

## Elkhound

The source code and build instructions for Elkhound are available at
[GitHub](https://github.com/WeiDUorg/elkhound). There are also
compiled executables for some platforms available under
[Releases](https://github.com/WeiDUorg/elkhound/releases/latest). Windows
builds without a Cygwin-dependency, as well as ARM builds for MacOS
can be obtained
[here](https://github.com/The-Mod-Elephant/elkhound/releases).

## Compiling WeiDU

### First time compiling

- Obtain WeiDU's source code. The recommended way is by using git:
  `git clone git://github.com/WeiDUorg/weidu.git your/directory` Bear
  in mind WeiDU builds distribution packages to the directory one
  level up from where the source is located.

### If you have compiled before

- Make sure you have the up-to-date WeiDU source. The recommended way
  is by using git (from inside the directory where you keep your WeiDU
  source code): `git pull origin`

### Finally

- Check out the branch from which you wish to compile WeiDU. If you
  are building a stable version, check out the `master` branch. If
  your are building a beta version, check out the `devel` branch. From
  inside your WeiDU source directory, you check out the branch with:
  `git checkout branch`, where branch is the branch you wish to check
  out.

- Run make. Relevant build targets are
 * clean
 * weidu
 * weinstall
 * tolower
 * doc
 * windows_zip
 * linux_zip
 * osx_zip
 * src_zip

The *_zip targets produce an archive in `..` that is suitable for
distribution. If you are not developing WeiDU, you probably want one
of windows_zip, linux_zip or osx_zip.

## Sandbox mode

Sandbox mode is provided by `scripts/weidu-sandbox.py`, a Python 3.10+
launcher. `make weidu` still builds the normal WeiDU binary; it does not
build a separate `weidu-sandbox` binary. The launcher builds or runs a
Docker or Podman image that contains WeiDU, copies the selected game
directory to a temporary host directory, mounts that temporary copy plus a
private diagnostics directory inside the container, and reports the file
changes observed in the copy.

Normal WeiDU installation does not require Python, Docker, or Podman. The
sandbox launcher requires Python 3.10 or newer plus either Docker or
Podman.

### What the sandbox does

- Copies the game directory to a temporary location.
- Runs WeiDU in a container with no network access.
- Mounts the temporary game copy and a private temporary diagnostics
  directory, not the real game directory.
- Writes a JSON report when `--report` is provided.
- Runs WeiDU under `strace` inside the container and adds diagnostics for
  outside-game file syscall attempts.
- Deletes the temporary game copy after the run unless `--keep-temp` is
  provided.

The sandbox is for inspection. To actually install a mod after reviewing
the report, run the normal WeiDU command against the real game directory.

### Usage

Run the launcher from this WeiDU checkout or release directory, or invoke
`scripts/weidu-sandbox.py` by its full path. The launcher finds the
source checkout, or the packaged `sandbox-src/` directory in release
archives, relative to the script location. Keep `scripts/` and
`sandbox-src/` together if you move files out of an extracted release.
The shell's current directory is not important for image building. You do
not need a host `weidu` executable in `PATH` or copied into the game
directory for a sandbox run; the container image supplies WeiDU.

Point `--game` at the real game directory. That directory should already
contain the mod files as it would for a normal WeiDU install. The TP2 may
be either in the game directory, such as `setup-mymod.tp2`, or inside the
mod directory, such as `mymod/setup-mymod.tp2`. The sandbox copies that
game directory, starts WeiDU inside the copy with `/game` as its working
directory, and resolves all arguments after `--` inside that temporary
game copy. Paths such as `--report sandbox-report.json` are host paths
resolved from the shell's current directory.

Build the local sandbox image and run WeiDU once:

```bash
python3 scripts/weidu-sandbox.py \
  --game /path/to/game \
  --build-image \
  --report sandbox-report.json \
  -- --force-install 0 setup-mymod.tp2
```

If the mod keeps its TP2 inside the mod folder, pass that relative path to
WeiDU:

```bash
python3 scripts/weidu-sandbox.py \
  --game /path/to/game \
  --report sandbox-report.json \
  -- --force-install 0 mymod/setup-mymod.tp2
```

After the image already exists, omit `--build-image`:

```bash
python3 scripts/weidu-sandbox.py \
  --game /path/to/game \
  --report sandbox-report.json \
  -- --force-install 0 setup-mymod.tp2
```

On Windows, use `py` or `python` instead of `python3`. For example, if
WeiDU was extracted to the usual release directory:

```powershell
py "C:\Users\whatever\Downloads\WeiDU-Windows\scripts\weidu-sandbox.py" `
  --game "C:\Program Files (x86)\Steam\steamapps\common\Icewind Dale Enhanced Edition" `
  --build-image `
  --report sandbox-report.json `
  -- --force-install 0 setup-mymod.tp2
```

If your terminal is already in the game directory, invoke the launcher by
full path and use `--game .`:

```powershell
cd "C:\Program Files (x86)\Steam\steamapps\common\Icewind Dale Enhanced Edition"
py "C:\Users\whatever\Downloads\WeiDU-Windows\scripts\weidu-sandbox.py" `
  --game . `
  --report sandbox-report.json `
  -- --force-install 0 setup-mymod.tp2
```

### Windows setup

Install Python 3.10 or newer. The clearest option is to install it from
<https://www.python.org/downloads/windows/> and then open a new
PowerShell window. If you prefer a command-line install, use WinGet:

```powershell
winget install -e --id Python.Python.3.14
python --version
```

Install one container runtime. Docker Desktop is the most familiar option:

```powershell
winget install -e --id Docker.DockerDesktop
```

Start Docker Desktop from the Start menu, accept its terms, then verify:

```powershell
docker version
```

Podman is also supported:

```powershell
winget install -e --id RedHat.Podman
podman machine init
podman machine start
podman info
```

### macOS setup

Install Python 3.10 or newer. The Python.org installer is the most direct
option; Homebrew is also fine if you already use it:

```bash
brew install python
python3 --version
```

Install one container runtime. For Docker Desktop, download `Docker.dmg`
from Docker and install it from Terminal:

```bash
sudo hdiutil attach Docker.dmg
sudo /Volumes/Docker/Docker.app/Contents/MacOS/install
sudo hdiutil detach /Volumes/Docker
open -a Docker
docker version
```

Podman is also supported:

```bash
brew install podman
podman machine init
podman machine start
podman info
```

### Linux setup

Install Python 3.10 or newer and Podman with your distribution package
manager:

```bash
# Debian/Ubuntu
sudo apt-get update
sudo apt-get install -y python3 podman

# Fedora/CentOS/RHEL
sudo dnf install -y python3 podman

# Arch/Manjaro
sudo pacman -S python podman

# Alpine
sudo apk add python3 podman
```

Verify the tools:

```bash
python3 --version
podman info
```

Docker Engine is also supported on Linux, but its package repository setup
varies by distribution. Use Docker's current Linux installation
instructions if you prefer Docker over Podman.

### Runtime notes

- The launcher auto-detects Docker or Podman. Use `--runtime docker` or
  `--runtime podman` to force one.
- Docker Desktop licensing may matter for commercial users.
- On Windows and macOS, Docker and Podman run Linux containers through a
  local VM. The sandbox runs Linux WeiDU, so unconditional Windows-only
  or macOS-only shell commands may behave differently than in a native
  install.
- The JSON report's `diagnostics.syscall_trace` section is parsed from
  `strace -f -e trace=file` output collected inside the container. It can
  reveal silent file writes, deletes, renames, mkdirs, chmods, and
  write-mode opens even when shell errors are redirected to `/dev/null`.
- The JSON report's `diagnostics.outside_game_access` section combines
  captured stdout/stderr evidence with the syscall trace. Absolute paths
  outside `/game`, parent traversals such as `../outside.txt`, and
  Windows-style absolute path syntax are reported there when visible to
  either source.
- Writes to `/tmp` happen inside the container's ephemeral tmpfs. They are
  discarded after the run and are reported separately from outside-game
  access when visible in output or syscall trace.
- Official installation references:
  - Python: <https://docs.python.org/3/using/>
  - Docker Desktop: <https://docs.docker.com/desktop/>
  - Docker Engine on Linux: <https://docs.docker.com/engine/install/>
  - Podman: <https://podman.io/docs/installation>
