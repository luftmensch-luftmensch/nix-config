{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.services.battery;
  battery = pkgs.writeShellApplication {
    name = "battery";
    runtimeInputs = with pkgs; [
      bash
      coreutils
      busybox
      acpi
      glib.bin
      libnotify
    ];
    text =
      let
        _acpi = "${lib.getExe pkgs.acpi} -b";
        _notify = "${lib.getExe pkgs.libnotify} -t 1000 -r 1 -u critical";
        _logger = "${lib.getExe pkgs.logger} -t battery-status -p warning";
      in
      ''
        send_notification() {
          ${_notify} -i "$1" "$2" || ${_logger} "Failed to send notification for battery status"
        }

        capacity="$(${_acpi} | awk '!/(rate information unavailable)/' | awk NR==1 | grep -io "[0-9]*%" | sed 's/%//g' )"
        status="$(${_acpi} | awk 'NR==1 {print $3}' | sed 's/,//g')"

        if [ "$capacity" -le 20 ] && [ "$capacity" -ge 11 ] && [ "$status" == "Discharging" ]; then
            send_notification battery_low "$capacity %: See you, space cowboy..." 
        elif [ "$capacity" -le 10 ] && [ "$status" == "Discharging" ]; then
            send_notification battery_caution "Running on low battery!"
        elif [ "$capacity" -eq 100 ] && [ "$status" == "Full" ]; then
             send_notification battery "Fully recharged! Please remove the power supply"
        fi
      '';
  };
in
{
  options.valentino.modules.services.battery = {
    enable = mkEnableOption "battery checker service";
    interval = mkOption {
      default = "5min";
      type = types.nullOr types.str;
      example = "1h";
      description = "Interval of spawn";
    };
  };

  config = mkIf cfg.enable {
    systemd.user = {
      services.battery = {
        Unit = {
          Description = "Battery Status Checker";
          StartLimitIntervalSec = 3;
          StartLimitBurst = 1;
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${lib.getExe battery}";
          IOSchedulingClass = "idle";
        };
        Install.WantedBy = [ "graphical-session.target" ];
      };

      timers.battery = {
        Unit.Description = "Timer for battery status service";
        Timer.OnUnitActiveSec = cfg.interval;
        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
