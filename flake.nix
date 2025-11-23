{
  description = "weidu";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/2cd3cac16691a933e94276f0a810453f17775c28";
  };
  outputs = { self, nixpkgs }:
    let systems = [
      "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"
    ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            name = "weidu-shell";
            src = self;
            # Libs
            buildInputs = with pkgs; [
              elkhound
              gnumake
              ocaml-ng.ocamlPackages_4_14_unsafe_string.ocaml
              perl
              which
            ];
            # Tools
            nativeBuildInputs = with pkgs; [
              git
              hadolint
            ];
            # Env
            shellHook = ''
            '';
          };
      });
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in pkgs.stdenv.mkDerivation {
          pname = "weidu";
          version = "v250";

          src = ./.;

          nativeBuildInputs = with pkgs; [
            elkhound
            gnumake
            ocaml-ng.ocamlPackages_4_14_unsafe_string.ocaml
            perl
            which
          ];

          buildInputs = with pkgs; [
            git
          ];

          configurePhase = ''
            cp sample.Configuration Configuration
            make clean
            mkdir -p $out/bin
          '';

          buildPhase = ''
            make
          '';

          installPhase = ''
            mv weidu.asm.exe $out/bin/.
          '';
          enableParallelBuilding = false;
        }
      );
    };
}
