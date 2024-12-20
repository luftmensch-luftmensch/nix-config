{ inputs, ... }:
let
  inherit (inputs.nixpkgs) lib;
in
rec {
  supportedSystems = [
    "x86_64-linux"
    "aarch64-linux"
  ];
  forAllSystems = lib.genAttrs supportedSystems;

  mkSystem = import ./system.nix { inherit inputs; };
  mkHome = import ./home.nix { inherit inputs; };
  whenT = k: t: if k then t else [ ];
}
