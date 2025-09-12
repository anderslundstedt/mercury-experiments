# flake based on
# https://github.com/NixOS/templates/blob/ad0e221dda33c4b564fad976281130ce34a20cb9/bash-hello/flake.nix
{
  description = "Experiments with the Mercury language";

  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  outputs = inputs@{self, nixpkgs}: (
    let

      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      # helper function to generate an attrset
      # '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      get-nixpkgs-for-system = system: (
        import inputs.nixpkgs {inherit system;}
      );
    in {
      devShell = forAllSystems(sys:
        let
          nixpkgs = get-nixpkgs-for-system sys;
        in
        nixpkgs.mkShell {
          buildInputs = [
            nixpkgs.bash
            nixpkgs.gnumake
            nixpkgs.mercury
          ];
        }
      );
    }
  );
}
