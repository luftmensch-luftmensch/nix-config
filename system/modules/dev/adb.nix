{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.adb;
in {
  options.system.modules.dev.adb = {
    enable = mkEnableOption "Enable adb";
  };

  config = mkIf cfg.enable {
    programs.adb.enable = true;
  };
}
