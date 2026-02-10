{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.xorg;
in
{
  options.valentino.modules.xorg.enable = mkEnableOption "xorg configuration management for user";

  config = mkIf cfg.enable {
    xsession.enable = true;

    home.packages = with pkgs; [
      scrot
      xclip
      xdotool

      xautolock
      xclip
      xorg.xev
      xorg.xkill
      xorg.xrdb
      xss-lock
      xfce.xfce4-screenshooter
    ];

    valentino.modules.xorg = {
      locker.enable = true;
      picom.enable = true;
      clipcat.enable = true;
    };
  };
}
