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

      # I have to start IntelliJ and other XWayland apps with GDK_SCALE-1 GDK_DPI_SCALE=0.5 idea.sh. Not as crisp as native Wayland applications but way better than before.
	  # TODO: Enable it when needed
      # (androidStudioPackages.dev.override {tiling_wm = true;})
    ];
  };
}
