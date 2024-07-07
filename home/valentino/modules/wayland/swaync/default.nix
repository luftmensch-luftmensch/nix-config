{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.wayland.swaync;
  inherit (config.valentino.modules) wayland;
  inherit (config.colorScheme) palette;
  theme = config.valentino.modules.themes;
in
{
  options.valentino.modules.wayland.swaync = {
    enable = mkEnableOption "swaync configuration";
    package = lib.mkPackageOption pkgs "swaynotificationcenter" { };
  };

  config = mkIf wayland.enable {
    home.packages = [ cfg.package ];
    services.swaync =
      let
        config = import ./config.nix {
          inherit
            lib
            theme
            palette
            pkgs
            ;
        };
      in
      {
        enable = true;
        package = cfg.package;
        inherit (config) settings style;
      };
  };
}
