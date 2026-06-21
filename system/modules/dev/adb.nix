{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.system.modules.dev.adb;
in
{
  options.system.modules.dev.adb.enable = mkEnableOption "Enable adb";

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.android-tools ];
  };
}
