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
  palette = config.stylix.base16Scheme;
in
{
  config = mkIf cfg.enable {
    stylix.targets.sway.enable = true;

    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      systemd.enable = true;

      config =
        let
          settings = import ./settings.nix {
            inherit
              lib
              config
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

      extraConfig =
        let
          _wob_sock =
            lib.replaceStrings [ "%t" ] [ "$XDG_RUNTIME_DIR" ]
              config.systemd.user.sockets.wob.Socket.ListenFIFO;
        in
        ''
          set {
            $opacity 0.9
            $wob_sock ${_wob_sock}
          }
          bindgesture swipe:3:right workspace back_and_forth
          bindgesture swipe:3:left workspace back_and_forth
          bindgesture swipe:3:up fullscreen
        '';
    };

    home.packages = with pkgs; [
      autotiling
      wdisplays
    ];

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
        wob.enable = true;
      };
      apps.playerctl.enable = true;
      services.battery.enable = true;
    };
  };
}
