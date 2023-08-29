{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.adb;
in {
  options.system.modules.dev.adb = {
    enable = mkEnableOption "Enable adb & related pkgs";
  };

  config = mkIf cfg.enable {
    programs.adb.enable = true;
    environment.systemPackages = with pkgs; [
      jmtpfs  # sudo jmtpfs -o allow_other ~/DIRECTORY
      gphoto2 # digital camera software applications
      scrcpy  # Display & control Android device
    ];
  };
}
