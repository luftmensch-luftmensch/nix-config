{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.core.boot;
in {
  options.system.modules.core.boot = {
    quietboot.enable = mkEnableOption "quietboot";
  };

  config = mkIf cfg.quietboot.enable {
    boot.plymouth = {
      enable = true;
      # theme = "bgrt";
    };

    boot = {
      initrd.verbose = false;
      consoleLogLevel = 0;
    };
    # Silent boot. (Taken from -> https://wiki.archlinux.org/title/Silent_boot)
    boot.kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "udev.log_priority=3"
      "loglevel=3"
      "rd.systemd.show_status=false"
      "rd.udev.log_level=3"
      "vt.global_cursor_default=0"
    ];

    console = {
      useXkbConfig = true;
      earlySetup = false;
    };
  };
}
