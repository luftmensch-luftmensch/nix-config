{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.wayland.locker;
  inherit (config.wayland.windowManager) sway hyprland;
  inherit (config.valentino.modules) themes;
  inherit (config.colorScheme) palette;
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
        font = themes.font.regular.family;
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

        layout-text-color = palette.base0E;

        bs-hl-color = palette.base08;
        key-hl-color = palette.base0B;
        separator-color = palette.base05;

        text-color = palette.base07;
        text-clear-color = palette.base07;
        text-ver-color = palette.base07;
        text-wrong-color = palette.base07;

        inside-color = transparent;
        inside-clear-color = transparent;
        inside-ver-color = transparent;
        inside-wrong-color = transparent;

        line-color = transparent;
        line-clear-color = transparent;
        line-ver-color = transparent;
        line-wrong-color = transparent;

        ring-color = palette.base01;
        ring-clear-color = palette.base0A;
        ring-ver-color = palette.base0E;
        ring-wrong-color = palette.base08;
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
        ++ (optionals sway.enable
          [
            {
              timeout = 360;
              command = "${swaymsg} output * dpms off";
              resumeCommand = "${swaymsg} output * dpms on";
            }
          ])
        ++ (optionals hyprland.enable [
          {
            timeout = 360;
            command = "${hyprctl} dispatch dpms off";
            resumeCommand = "${hyprctl} dispatch dpms on";
          }
        ]);
    };

    systemd.user.services.swayidle.Install = {
      WantedBy =
        (optionals sway.enable ["sway-session.target"])
        ++ (optionals hyprland.enable ["hyprland-session.target"]);
    };
  };
}
