{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg;
  theme = config.valentino.modules.themes;
  mod = "Mod4";
  mod1 = "Mod1";
in {
  config = mkIf (cfg.enable && cfg.wm == "i3") {
    xsession = {
      enable = true;
      initExtra = "xset b off";
      windowManager.i3 = let
        settings = import ./settings.nix {
          default_mod = "${mod}";
          alt_mod = "${mod1}";
          inherit theme pkgs;
        };
      in {
        enable = true;
        config = {
          modifier = "${mod}";
          floating.modifier = "${mod}";
          workspaceAutoBackAndForth = true;

          inherit (settings) assigns gaps keybindings modes startup;
        };

        inherit (settings) extraConfig;
      };
    };

    home.packages = with pkgs; [
      autotiling
      pamixer
    ];

    services = {
      pasystray.enable = true;
      # Random background using feh
      random-background = {
        enable = true;
        display = "scale";
        imageDirectory = "%h/Dropbox/Immagini/Anime";
        interval = "15min";
      };
    };

    valentino.modules = {
      apps = {
        dunst.enable = true;
        playerctl.enable = true;
        polybar = {
          enable = true;
          temperature = "/sys/class/thermal/thermal_zone2/temp";
        };
      };
      xorg.xob.enable = true;
    };
  };
}
