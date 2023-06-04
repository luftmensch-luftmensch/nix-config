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
      chromium                 # Basic packages without custom command line flags
      dmidecode                # BIOS hw info tool
      linuxPackages.cpupower   # Change the frequency limits of your cpu and its governor
      feh                      # Image viewer and cataloguer
      pamixer                  # Pulseaudio command line mixer
      parcellite               # Lightweight GTK+ Clipboard Manager
      picom                    # X11 Compositor
      polybarFull              # A fast and easy-to-use tool status bar
      plymouth                 # Send commands to plymouthd
      scrot                    # Screenshot utility on Xorg

      xautolock                # Fire  up programs  in case  of user  inactivity under X
      xclip                    # Command line interface to X selections (clipboard)
      xorg.xev                 # Print contents of X events
      xorg.xkill               # Kill a client by its X resource
      xorg.xrdb                # X server resource database utility
      xss-lock                 # Use external locker as X screen saver
      xfce.xfce4-screenshooter # Screenshoter utility
    ];
    
  };
}
