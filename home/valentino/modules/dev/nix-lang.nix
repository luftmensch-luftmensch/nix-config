{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.dev.nix;
in
{
  options.valentino.modules.dev.nix.enable = mkEnableOption "nix language extra tools and language server";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      statix
      deadnix
      alejandra
      nixd
      nix-output-monitor
      nixpkgs-review
      nvd
      nix-prefetch-git
      nix-search-cli
    ];
  };
}
