# Building WeiDU

## Requirements and build environment

The version of OCaml needs to be greater than or equal to 4.04 and
less than or equal to 4.11, and configured without forced safe
strings.

### GNU/Linux

- OCaml with native compilers.

- A basic GCC tool chain with make.

- Perl, which is normally installed by default.

- Optionally, also git, hevea, texlive, zip and upx. HeVeA and TexLive
  are only needed to build the documentation and zip and upx are used
  in making the distributable archives (upx is optional).

- Elkhound (*vide infra*). Place the executable on you path and allow
  Elkhound to be executed as a program, for example, by using the
  terminal command `chmod +x path/to/elkhound`

### Windows

- Native OCaml (typically compiled by MinGW), obtained from ocaml.org.

- A Cygwin-based *nix tool chain, particularly `binutils` and `make`
  from the `Devel` group. Perl is also required but is typically
  installed by default. Optionally also `git` (Devel), `openssh`
  (Net), `zip` (Archive) and `upx` (Utils).

- Cygwin-hosted MinGW-GCC, called `mingw-gcc-core`, or some such.

- Elkhound (*vide infra*). For less configuration, place the elkhound
  binary in Cygwin's `/bin` directory, or equivalent. The build
  process does not like paths with spaces.

### MacOS

- Make sure you have Xcode installed. You probably don't need Xcode
  *per se* but apparently it is the conventional way of obtaining some
  of the necessary programs (make, gcc, etc.). You may be able to
  download and install Xcode's command-line tools from [Apple
  Developer](https://developer.apple.com/downloads/) without having to
  download and install all of Xcode.

- Install [MacPorts](https://www.macports.org/index.php) or
  [HomeBrew](http://brew.sh/).

- Install Perl (using MacPorts or HomeBrew, for example).

- Use MacPorts or HomeBrew to install OCaml. Note where OCaml was
  installed.

- Optionally, install UPX. UPX is used to compress the compiled
  programs, but is not available for all platforms.

- Obtain Elkhound (*vide infra*) and place the executable on your
  path. Allow Elkhound to be executed as a program, for example, by
  using the terminal command `chmod +x path/to/elkhound`

## Elkhound

The source code and build instructions for Elkhound are available at
[GitHub](https://github.com/WeiDUorg/elkhound). There are also
compiled executables for some platforms available under
[Releases](https://github.com/WeiDUorg/elkhound/releases/latest).

## Compiling WeiDU

### First time compiling

- Obtain WeiDU's source code. The recommended way is by using git:
  `git clone git://github.com/WeiDUorg/weidu.git your/directory` Bear
  in mind WeiDU builds distribution packages to the directory one
  level up from where the source is located.

- Enter the directory where you put WeiDU's source code. Copy the file
  ./sample.Configuration to ./Configuration. Open the file and change
  the value for `OCAMLDIR` for your platform to the directory in which
  the OCaml binaries are located.

### If you have compiled before

- Make sure you have the up-to-date WeiDU source. The recommended way
  is by using git (from inside the directory where you keep your WeiDU
  source code): `git pull origin`

- If the file ./sample.Configuration has been changed, recreate
  ./Configuration and re-apply any changes you have made to it.

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
