{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.xorg-related;
in {
  options.system.modules.packages.xorg-related = {
    enable = mkEnableOption "Enable packages running only under xorg";
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # chromium                 # Basic packages without custom command line flags
      dmidecode                # BIOS hw info tool
      linuxPackages.cpupower   # Change the frequency limits of your cpu and its governor
      # parcellite               # Lightweight GTK+ Clipboard Manager
      # picom                    # X11 Compositor
      # polybarFull              # A fast and easy-to-use tool status bar
      plymouth                 # Send commands to plymouthd
    ];
    
  };
}
