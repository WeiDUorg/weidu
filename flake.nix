{
  description = "elkhound";

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
              bison
              cmake
              elkhound
              ocaml-ng.ocamlPackages_4_10.ocaml.overrideAttrs (old: {
                  configureFlags = old.configureFlags ++ [
                    # https://github.com/WeiDUorg/weidu/issues/197
                    "--disable-force-safe-string"]
              })
              flex
              gnumake
            ];
            # Tools
            nativeBuildInputs = with pkgs; [
              git
            ];
            # Env
            shellHook = ''
            '';
          };
        });
    };
}
