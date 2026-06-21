{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.wayland.locker;
  inherit (config.wayland.windowManager) sway;
  inherit (config.valentino.modules) themes;
in
{
  options.valentino.modules.wayland.locker.enable = mkEnableOption "wayland screen locker";

  config = mkIf cfg.enable {
    stylix.targets.swaylock.enable = true;

    programs.swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;
      settings = {
        font = themes.font.regular.family;
        datestr = "%a, %d-%m-%Y";
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
      };
    };

    services.swayidle =
      let
        swaylock = "${lib.getExe pkgs.swaylock-effects} -fF";
        swaymsg = "${pkgs.sway}/bin/swaymsg";
      in
      {
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

        timeouts = [
          {
            timeout = 300;
            command = "${swaylock}";
          }

          {
            timeout = 360;
            command = "${swaymsg} output * dpms off";
            resumeCommand = "${swaymsg} output * dpms on";
          }

          # {
          #   timeout = 360;
          #   command = "${swaymsg} 'output * dpms off'";
          #   resumeCommand = "${swaymsg} 'output * dpms on'";
          # }
        ];
      };

    systemd.user.services.swayidle.Install.WantedBy = (optionals sway.enable [ "sway-session.target" ]);
  };
}
