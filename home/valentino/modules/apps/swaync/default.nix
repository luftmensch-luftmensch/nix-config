{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  inherit (config.valentino.modules) wayland;
in {
  options.valentino.modules.apps.swaync = {
    enable = mkEnableOption "swaync configuration";
  };

  config = mkIf wayland.enable {
    home.packages = with pkgs; [swaynotificationcenter];

    home.file = {
      "swaync-config" = {
        source = ./config.json;
        target = ".config/swaync/config.json";
      };
      "swaync-style" = {
        source = ./style.css;
        target = ".config/swaync/style.css";
      };
      "swaync-scheme" = {
        source = ./configSchema.json;
        target = ".config/swaync/configSchema.json";
      };
    };
  };
}
