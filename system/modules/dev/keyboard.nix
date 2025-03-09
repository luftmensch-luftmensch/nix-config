{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.system.modules.dev.keyboard;
in
{
  options.system.modules.dev.keyboard.enable = mkEnableOption "Keyboard configuration tools";

  config = mkIf cfg.enable {
    hardware.keyboard.qmk.enable = true;
    environment.systemPackages = with pkgs; [
      qmk
      qmk-udev-rules
      qmk_hid
      via
    ];
    services.udev.packages = [ pkgs.via ];
  };
}
