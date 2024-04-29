{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.android;
in {
  options.valentino.modules.dev.android.enable = mkEnableOption "android devices support";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      jmtpfs
      gphoto2
      scrcpy
    ];
  };
}
