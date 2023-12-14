{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg;
in {
  options.valentino.modules.xorg = {
    enable = mkEnableOption "xorg configuration management for user";

    wm = mkOption {
      description = "An option to choose the window manager [xorg] configuration to enable";
      default = null;
      type = types.nullOr (types.enum ["i3"]);
      example = "i3";
    };
  };

  config = mkIf cfg.enable {
    xsession.enable = true;

    home.packages = with pkgs; [
			parcellite
      scrot
      xclip
      xdotool

      xautolock                # Fire  up programs  in case  of user  inactivity under X
      xclip                    # Command line interface to X selections (clipboard)
      xorg.xev                 # Print contents of X events
      xorg.xkill               # Kill a client by its X resource
      xorg.xrdb                # X server resource database utility
      xss-lock                 # Use external locker as X screen saver
      xfce.xfce4-screenshooter # Screenshoter utility
    ];

    valentino.modules.xorg = {
      locker.enable = true;
      picom.enable = true;
    };
  };
}
