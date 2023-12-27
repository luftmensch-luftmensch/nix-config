{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.wayland;
  theme = config.valentino.modules.themes;
  mod = "Mod4";
  mod1 = "Mod1";
  wallpaper_path = "${config.home.homeDirectory}/Dropbox/Immagini/backgrounds/Art/";
in {
  config = mkIf (cfg.enable && (elem "sway" cfg.wm)) {
    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      systemd.enable = true;

      config = let
        settings = import ./settings.nix {
          default_mod = "${mod}";
          alt_mod = "${mod1}";
          inherit theme wallpaper_path pkgs;
        };
      in {
        bars = [{command = "waybar";}];
        modifier = "${mod}";
        floating = {
          modifier = "${mod}";
          border = 1;
        };
        gaps = {
          inner = 5;
          outer = 5;
          smartGaps = true;
          smartBorders = "on";
        };

        focus.followMouse = true;

        fonts = {
          names = [theme.font.regular.family];
          # Sum required: floating point value but int option defined
          size = theme.font.regular.size + 0.0;
        };

        workspaceAutoBackAndForth = true;
        inherit (settings) input keybindings modes workspaceOutputAssign window startup;
      };

      extraConfig = ''
        set {
          $laptop eDP-1
          $monitor HDMI-A-1
          $externalmonitor DP-5
          $opacity 0.9
          # Wob - Overlay volume/backlight/progress/anything bar for Wayland
          $wob_sock $XDG_RUNTIME_DIR/wob.sock
        }
        bindgesture swipe:3:right workspace back_and_forth
        bindgesture swipe:3:left workspace back_and_forth
        bindgesture swipe:3:up fullscreen
      '';
    };

    home.packages = with pkgs; [autotiling];

    services = {
      easyeffects.enable = true;
      # Try out if there is some problems with it on wayland
      pasystray.enable = true;
    };

    valentino.modules = {
      wayland = {
        locker.enable = true;
        waybar.enable = true;
      };
    };
  };
}
