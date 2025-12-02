# WeiDU
[![](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)](https://github.com/The-Mod-Elephant/weidu/releases/latest)
[![](https://img.shields.io/badge/Windows-0078D6?&style=for-the-badge&logoColor=white&logo=git-for-windows)](https://github.com/The-Mod-Elephant/weidu/releases/latest)
[![](https://img.shields.io/badge/mac%20os-grey?style=for-the-badge&logo=apple&logoColor=white)](https://github.com/The-Mod-Elephant/weidu/releases/latest)
[![](https://img.shields.io/github/actions/workflow/status/The-Mod-Elephant/weidu/main.yaml?style=for-the-badge)](https://github.com/The-Mod-Elephant/weidu/actions/workflows/main.yaml)
[![](https://img.shields.io/github/license/The-Mod-Elephant/weidu?style=for-the-badge)](./LICENSE)

Please note this an unofficial version. Find the old version [here](https://github.com/WeiDUorg/weidu).

## Building weidu

## Requirements and build environment

The version of OCaml needs to be greater than or equal to 4.04 and less than or equal to 4.11, and configured without forced safe strings.

### GNU/Linux

- OCaml with native compilers. Generally we recommend [opam](https://opam.ocaml.org/) to get started. You'll need to create a switch because weidu uses unsafe strings:

```sh
opam switch create 4.08.1+default-unsafe-string
```

- A basic GCC tool chain with make. This should come preinstalled on most versions of linux.

- Perl, which is normally installed by default.

- Optionally, also git and hevea, texlive. HeVeA and TexLive are only needed to build the documentation.

- Elkhound (*vide infra*). Place the executable on you path and allow Elkhound to be executed as a program, for example, by using the terminal command `chmod +x path/to/elkhound`. You can find a pre-built version of elkhound [here](https://github.com/The-Mod-Elephant/elkhound/releases/latest).

### Nix

It is possible to build weidu via, nix. We have included a `flake.nix` file, which will automatically install all the tools and systems required. You will have to enable flakes once you have installed nix. Please note this is not available in windows.

To enable flakes in Nix, you need to add the following line to your configuration file: 

```sh
experimental-features = nix-command flakes
```

In either `~/.config/nix/nix.conf` for user-specific settings or `/etc/nix/nix.conf` for system-wide settings. After that, you can use the nix command with flakes features enabled.

#### Build with nix

Building executables (for linux and macos) is very straight forward with nix.

For linux:

```sh
nix build '.#packages.x86_64-linux'
```

For macos:

```sh
nix build '.#packages.aarch64-darwin'
```

### Windows

- Native OCaml (typically compiled by MinGW), obtained from either from ocaml.org or via opam. Again this is tricky due to unsafe strings. Builds of OCaml before 4.14 don't exist for opam for 64 bit windows. So the following is required.

```sh
opam switch create --packages=ocaml-option-default-unsafe-string ocaml-variants.4.14.2+options
```

- A Cygwin-based *nix tool chain, particularly `binutils` and `make` from the `Devel` group. Perl is also required but is typically installed by default. Optionally also `git` (Devel).

- Cygwin-hosted MinGW-GCC, called `mingw-gcc-core`, or some such.

- Elkhound (*vide infra*). For less configuration, place the elkhound binary in Cygwin's `/bin` directory, or equivalent. The build process does not like paths with spaces.

### MacOS

- OCaml with native compilers. Generally we recommend [opam](https://opam.ocaml.org/) to get started. You'll need to create a switch because weidu uses unsafe strings:

```sh
opam switch create 4.08.1+default-unsafe-string
```

- Install Perl (using MacPorts or HomeBrew, for example).

- Obtain Elkhound (*vide infra*) and place the executable on your path. Allow Elkhound to be executed as a program, for example, by using the terminal command `chmod +x path/to/elkhound`

## Elkhound

The source code and build instructions for Elkhound are available at
[GitHub](https://github.com/The-Mod-Elephant/elkhound).

There are also compiled executables for some platforms available under [Releases](https://github.com/The-Mod-Elephant/elkhound).

## Compiling WeiDU

### First time compiling

- Obtain WeiDU's source code. The recommended way is by using git: `git clone git://github.com/WeiDUorg/weidu.git your/directory` Bear in mind WeiDU builds distribution packages to the directory one level up from where the source is located.

- Enter the directory where you put WeiDU's source code. Copy the file:
```sh
./sample.Configuration
```
to
```sh
./Configuration
```

### If you have compiled before

- Make sure you have the up-to-date WeiDU source. The recommended way is by using git (from inside the directory where you keep your WeiDU source code): 

```sh
git reset origin/devel --hard
```

- If the file ./sample.Configuration has been changed, recreate ./Configuration and re-apply any changes you have made to it.

### Finally

- Check out the branch from which you wish to compile WeiDU.

```sh
git checkout devel
```

- Run make. Relevant build targets are
 * clean
 * weidu
 * weinstall
 * tolower
 * doc
 * windows_zip
 * linux_zip
 * osx_zip

## Using Weidu

### Environment Variables

- `WEIDU_VER` A variable that contains the version of the WeiDU binary (e.g., 25000).
- `WEIDU_EXECUTABLE` A variable that contains the full path of the current WeiDU binary.
- `WEIDU_OS` A variable that is set to the users current os (e.g., unix).
- `WEIDU_ARCH` A variable that is set to the users current cpu architecture (e.g., amd64).

This is handled in [here](src/arch2.ml). And maybe disabled via the:

```ocaml
ignore (Arch2.associate_these ())
```
set in some of the commands.
