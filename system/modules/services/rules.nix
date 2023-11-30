{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.services.udev-rules;
in {
  options.system.modules.services.udev-rules = {
    enable = mkEnableOption "Enable udev custom rules";
  };

  config = mkIf cfg.enable {
    services.udev = {
      extraRules = ''
        ACTION=="add", SUBSYSTEMS=="usb", SUBSYSTEM=="block", ENV{ID_FS_USAGE}=="filesystem", RUN{program}+="${pkgs.systemd}/bin/systemd-mount --no-block --automount=yes --collect $devnode /media"
      '';

      #Autosuspend usb
      #ACTION=="add", SUBSYSTEM=="usb", ATTR{power/control}="auto"
      #ACTION=="add", SUBSYSTEM=="usb", TEST=="power/autosuspend" ATTR{power/autosuspend}="120"

      # enable power control for all PCI devices
      #SUBSYSTEM=="pci", TEST=="power/control", ATTR{power/control}="auto"
      #SUBSYSTEM=="pci", TEST=="d3cold_allowed", ATTR{d3cold_allowed}="1"

      # amdgpu power saving
      # SUBSYSTEM=="drm", DRIVERS=="amdgpu", ATTR{device/power_dpm_force_performance_level}="manual"
      # SUBSYSTEM=="drm", DRIVERS=="amdgpu", ATTR{device/power_dpm_state}="battery"
      # SUBSYSTEM=="drm", DRIVERS=="amdgpu", ATTR{device/pp_power_profile_mode}="2"

      # enable power control for all USB devices except multifunction, HID and hubs (00, 03, 09) (https://www.usb.org/defined-class-codes)
      #SUBSYSTEM=="usb", TEST=="power/control", \
      #  ATTR{bInterfaceClass}!="00", ATTR{bInterfaceClass}!="03", ATTR{bInterfaceClass}!="09", \
      #  ATTR{bDeviceClass}!="00", ATTR{bDeviceClass}!="03", ATTR{bDeviceClass}!="09", \
      #  ATTR{power/control}="auto"
      #SUBSYSTEM=="usb", TEST=="power/control", \
      #  ENV{ID_USB_INTERFACES}!="", ENV{ID_USB_INTERFACES}!=":03*", ENV{ID_USB_INTERFACES}!=":09*" \
      #  ATTR{bDeviceClass}=="00", \
      #  ATTR{power/control}="auto"
    };
  };
}
