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
 * pcre2-source
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

The first WeiDU build fetches the exact PCRE2 revision recorded in
`pcre2/version`, verifies it, and prepares only the source files required by
WeiDU. The `pcre2-source` target performs this step explicitly. To build from
an existing PCRE2 checkout without network access, set `PCRE2_UPSTREAM_DIR` to
that checkout when invoking make.
