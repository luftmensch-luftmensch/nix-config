{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.wayland.locker;
  cfgSway = config.wayland.windowManager.sway;
  cfgHyprland = config.wayland.windowManager.hyprland;

  cfgTheme = config.valentino.modules.themes;
  inherit (config.colorScheme) colors;
in {
  options.valentino.modules.wayland.locker = {
    enable = mkEnableOption "wayland screen locker";
  };

  config = mkIf cfg.enable {
    programs.swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;
      settings = let
        transparent = "#00000000";
      in {
        font = cfgTheme.font.regular.family;
        datestr = "%a, %d-%M-%Y";
        effect-blur = "7x5";
        effect-vignette = "0.2:0.5";
        effect-scale = "0.4";
        fade-in = 1;
        screenshots = true;

        ignore-empty-password = true;
        hide-keyboard-layout = true;

        clock = true;
        indicator = true;
        indicator-radius = 150;
        indicator-thickness = 10;

        layout-text-color = colors.base0E;

        bs-hl-color = colors.base08;
        key-hl-color = colors.base0B;
        separator-color = colors.base05;

        text-color = colors.base07;
        text-clear-color = colors.base07;
        text-ver-color = colors.base07;
        text-wrong-color = colors.base07;

        inside-color = transparent;
        inside-clear-color = transparent;
        inside-ver-color = transparent;
        inside-wrong-color = transparent;

        line-color = transparent;
        line-clear-color = transparent;
        line-ver-color = transparent;
        line-wrong-color = transparent;

        ring-color = colors.base01;
        ring-clear-color = colors.base0A;
        ring-ver-color = colors.base0E;
        ring-wrong-color = colors.base08;
      };
    };

    services.swayidle = let
      swaylock = "${pkgs.swaylock-effects}/bin/swaylock -fF";
    in {
      enable = true;

      events = [
        {
          event = "before-sleep";
          command = "${swaylock}";
        }
        {
          event = "lock";
          command = "${swaylock}";
        }
      ];

      timeouts = let
        hyprctl = "${pkgs.hyprland}/bin/hyprctl";
        swaymsg = "${pkgs.sway}/bin/swaymsg";
      in
        [
          {
            timeout = 300;
            command = "${swaylock}";
          }
        ]
        ++ (optionals cfgSway.enable
          [
            {
              timeout = 360;
              command = "${swaymsg} output * dpms off";
              resumeCommand = "${swaymsg} output * dpms on";
            }
          ])
        ++ (optionals cfgHyprland.enable [
          {
            timeout = 360;
            command = "${hyprctl} dispatch dpms off";
            resumeCommand = "${hyprctl} dispatch dpms on";
          }
        ]);
    };

    systemd.user.services.swayidle.Install = {
      WantedBy =
        (optionals cfgSway.enable [
          "sway-session.target"
        ])
        ++ (optionals cfgHyprland.enable [
          "hyprland-session.target"
        ]);
    };
  };
}
