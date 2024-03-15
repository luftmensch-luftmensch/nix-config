{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.wayland.random-background;
  flags = lib.concatStringsSep " " ["-r" "-q" "output" "*"];
in {
  options.valentino.modules.wayland.random-background = {
    enable = mkEnableOption "wayland screen random background";

    imageDirectory = mkOption {
      type = types.str;
      example = "%h/backgrounds";
      description = ''
        The directory of images from which a background should be
        chosen. Should be formatted in a way understood by systemd,
        e.g., '%h' is the home directory.
      '';
    };

    display = mkOption {
      type = types.enum ["center" "fill" "max" "scale" "tile"];
      default = "fill";
      description = "Display background images according to this option.";
    };

    interval = mkOption {
      default = null;
      type = types.nullOr types.str;
      example = "1h";
      description = ''
        The duration between changing background image, set to null
        to only set background when logging in. Should be formatted
        as a duration understood by systemd.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      systemd.user.services.random-background = {
        Unit = {
          Description = "Set random desktop background using swaymsg";
          After = ["graphical-session-pre.target"];
          PartOf = ["graphical-session.target"];
        };

        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.sway}/bin/swaymsg ${flags} '\"$(${pkgs.findutils}/bin/find ${cfg.imageDirectory} -type f | shuf -n1)\"' ${cfg.display}";
          IOSchedulingClass = "idle";
        };

        Install.WantedBy = ["graphical-session.target"];
      };
    }
    (mkIf (cfg.interval != null) {
      systemd.user.timers.random-background = {
        Unit.Description = "Set random desktop background using swaymsg";
        Timer.OnUnitActiveSec = cfg.interval;
        Install.WantedBy = ["timers.target"];
      };
    })
  ]);
}
