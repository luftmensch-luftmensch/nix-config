{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.rofi;
  rofiFonts = pkgs.nerd-fonts.iosevka;
in
{
  options.valentino.modules.apps.rofi.enable = mkEnableOption "rofi configuration";

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      package = pkgs.rofi;
      plugins = [ pkgs.rofi-emoji ];
    };

    home.packages = [
      pkgs.rofi-powermenu
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
