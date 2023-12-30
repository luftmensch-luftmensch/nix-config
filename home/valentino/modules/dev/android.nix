{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.android;
in {
  options.valentino.modules.dev.android = {
    enable = mkEnableOption "android devices support";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      jmtpfs  # sudo jmtpfs -o allow_other ~/DIRECTORY
      gphoto2 # digital camera software applications
      scrcpy  # Display & control Android device
    ];
  };
}
