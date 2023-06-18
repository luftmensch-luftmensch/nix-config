{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.services.logind;
in {
  options.system.modules.services.logind = {
    enable = mkEnableOption "Enable logind capabilities";
  };

  config = mkIf cfg.enable {
    services.logind = {
      lidSwitch = "suspend"; #don't suspend when lid is closed (other options are poweroff reboot hatl kexec suspend hibernate hybrid-sleep suspend-then-hiberante lock)
      lidSwitchDocked = "poweroff";

      # idleaction with startx: https://bbs.archlinux.org/viewtopic.php?id=207536
      # <LeftMouse>https://wiki.archlinux.org/title/Power_management
      # Options: ttps://www.freedesktop.org/software/systemd/man/logind.conf.html

      #extraConfig = ''
      #  HandlePowerKey=suspend # or suspend-then-hibernate
      #  HandleSuspendKey=ignore
      #  HandleHibernateKey=ignore
      #  HandleLidSwitch=ignore
      #  HandleLidSwitchDocket=ignore
      #  IdleAction=suspend # or suspend-then-hibernate
      #  IdleActionSec=1m
      #'';
    };
  };
}
