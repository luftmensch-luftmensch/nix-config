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
  inherit (config.colorScheme) palette;
in {
  config = mkIf (cfg.enable && (elem "sway" cfg.wm)) {
    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      systemd.enable = true;

      config = let
        settings = import ./settings.nix {
          inherit mod mod1 theme palette wallpaper_path pkgs;
        };
      in {
        bars = [{command = "waybar";}];
        modifier = "${mod}";
        floating = {
          modifier = "${mod}";
          border = 1;
        };

        focus.followMouse = true;

        workspaceAutoBackAndForth = true;
        inherit (settings) input keybindings modes workspaceOutputAssign window startup gaps fonts;
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

    valentino.modules = {
      wayland = {
        locker.enable = true;
        waybar = {
          enable = true;
          default_output = "eDP-1";
          external_output = "HDMI-A-1";
        };
      };
      apps.playerctl.enable = true;
    };
  };
}
