{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.swaync;
  inherit (config.valentino.modules) wayland;
  inherit (config.colorScheme) palette;
  theme = config.valentino.modules.themes;
in {
  options.valentino.modules.apps.swaync = {
    enable = mkEnableOption "swaync configuration";
    package = lib.mkPackageOption pkgs "swaynotificationcenter" {};
  };

  config = mkIf wayland.enable {
    home.packages = [cfg.package];

    home.file = let
      schema = "${cfg.package}/etc/xdg/swaync/configSchema.json";
      config = import ./config.nix {inherit schema;};
      style = import ./style.nix {inherit theme palette;};
    in {
      ".config/swaync/config.json" = {
        enable = true;
        inherit (config) text;
      };

      ".config/swaync/style.css" = {
        enable = true;
        inherit (style) text;
      };
    };
  };
}
