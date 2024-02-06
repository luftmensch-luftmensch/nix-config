{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.rofi;
  inherit (config.valentino.modules) wayland;
  inherit (config.valentino.modules.credentials) bitwarden;
  rofiFonts = pkgs.nerdfonts.override {
    fonts = ["Iosevka"];
  };
in {
  options.valentino.modules.apps.rofi = {
    enable = mkEnableOption "rofi configuration";
  };

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      package =
        if wayland.enable
        then pkgs.rofi-wayland
        else pkgs.rofi;
      plugins = with pkgs; [rofi-emoji];
    };

    home.packages = with pkgs;
      [rofi-powermenu]
      ++ optionals bitwarden.enable [rofi-rbw]
      ++ [rofiFonts];
    xdg.configFile = {
      "rofi/colors/color.rasi".text = ''
        /*
         *
         * Change here you colorscheme
         *
         */

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

      "rofi-rbw.rc" = mkIf bitwarden.enable {
        text = ''
          action = type
          prompt = Select credentials
          selector-args = -theme ~/.config/rofi/themes/rbw
        '';
      };
    };
  };
}
