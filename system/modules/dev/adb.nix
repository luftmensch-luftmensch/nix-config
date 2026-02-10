{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.system.modules.dev.adb;
in
{
  options.system.modules.dev.adb.enable = mkEnableOption "Enable adb";

  # TODO: Since adb-udev-rule was dropped see: https://github.com/M0Rf30/android-udev-rules
  config = mkIf cfg.enable {
    programs.adb.enable = true;

    environment.systemPackages = with pkgs; [ docker ];
  };
}
