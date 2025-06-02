{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.rofi;
  inherit (config.valentino.modules) wayland;

  rofiPkg = if wayland.enable then pkgs.rofi-wayland else pkgs.rofi;
  rofi-emoji = if wayland.enable then pkgs.rofi-emoji-wayland else pkgs.rofi-emoji;
  rofi-powermenu = if wayland.enable then pkgs.rofi-powermenu-wayland else pkgs.rofi-powermenu;
  rofiFonts = pkgs.nerd-fonts.iosevka;
in
{
  options.valentino.modules.apps.rofi.enable = mkEnableOption "rofi configuration";

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      package = rofiPkg;
      plugins = [ rofi-emoji ];
    };

    home.packages = [
      rofi-powermenu
      rofiFonts
    ];

    xdg.configFile = {
      "rofi/colors/color.rasi".text = ''
        @import "${config.colorscheme.slug}.rasi"
      '';

      "rofi/colors" = {
        source = ./config/colors;
        recursive = true;
      };

      "rofi/themes" = {
        source = ./config/themes;
        recursive = true;
      };
    };
  };
}
