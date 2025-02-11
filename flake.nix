{
  description = "A fresh retake of the React API in Fable, optimized for happiness.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        {
          self',
          pkgs,
          system,
          ...
        }:
        let
          permittedPackages = [
            "dotnet-core-combined"
            "dotnet-wrapped-combined"
            "dotnet-combined"
            "dotnet-sdk-6.0.428"
            "dotnet-sdk-wrapped-6.0.428"
            "dotnet-sdk-6.0.136"
          ];
        in
        {
          _module.args.pkgs = import self.inputs.nixpkgs {
            inherit system;
            config = {
              permittedInsecurePackages = permittedPackages;
            };
          };
          devShells.default = pkgs.mkShell {
            inputsFrom = builtins.attrValues self'.packages;
            packages = with pkgs; [
              (
                with dotnetCorePackages;
                combinePackages [
                  dotnetCorePackages.sdk_6_0
                  dotnetCorePackages.sdk_8_0
                ]
              )
              fantomas
            ];
          };
        };
    };
}
