{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.dmenu-with-patches;
in {
  options.system.modules.packages.dmenu-with-patches = {
    enable = mkEnableOption "Dmenu w/ additional packages";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (dmenu.overrideAttrs (oldAttrs: {
        patches = [./dmenu-patches/case-insensitive ./dmenu-patches/borders];
      }))
    ];
  };
}
