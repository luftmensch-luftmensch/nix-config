{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.wayland;
  theme = config.valentino.modules.themes;
  mod = "Mod4";
  mod1 = "Mod1";
  default_output = "eDP-1";
  external_output = "HDMI-A-1";
  imageDirectory = "${config.home.homeDirectory}/Dropbox/Immagini/wallpapers/Art/";
  inherit (config.colorScheme) palette;
in
{
  config = mkIf (cfg.enable && (elem "sway" cfg.wm)) {
    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      systemd.enable = true;

      config =
        let
          settings = import ./settings.nix {
            inherit
              lib
              mod
              mod1
              default_output
              external_output
              theme
              palette
              pkgs
              ;
          };
        in
        {
          bars = [ ];
          modifier = "${mod}";
          floating = {
            modifier = "${mod}";
            border = 1;
          };

          focus.followMouse = true;

          workspaceAutoBackAndForth = true;
          inherit (settings)
            input
            output
            keybindings
            modes
            workspaceOutputAssign
            window
            startup
            gaps
            fonts
            ;
        };

      extraConfig = ''
        set {
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

    home.packages = [ pkgs.autotiling ];

    # xdg.portal = {
    #   enable = true;
    #    extraPortals = [pkgs.xdg-desktop-portal-wlr pkgs.xdg-desktop-portal-gtk];
    #    configPackages = [pkgs.xdg-desktop-portal-wlr];
    #    config = {
    #      common.default = ["*"];
    #      sway.default = ["gtk" "wlr"];
    #   };
    # };

    valentino.modules = {
      wayland = {
        locker.enable = true;
        random-background = {
          enable = true;
          display = "fill";
          interval = "15min";
          inherit imageDirectory;
        };

        waybar = {
          enable = true;
          battery.enable = true;
          inherit default_output external_output;
        };
      };
      apps.playerctl.enable = true;
    };
  };
}
