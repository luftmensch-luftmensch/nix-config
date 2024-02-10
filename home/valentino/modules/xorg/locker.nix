{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg.locker;
in {
  options.valentino.modules.xorg.locker = {
    enable = mkEnableOption "xorg screen locker";
  };

  config = mkIf cfg.enable {
    services.screen-locker = {
      enable = true;
      xautolock = {
        enable = true;
        extraOptions = ["-time 10"];
      };
      lockCmd = let
        ff-locker = pkgs.writeShellScriptBin "ff-locker" ''
          TMPBG=/tmp/screen.png
          [ -f "$TMPBG" ] && rm $TMPBG

          ${pkgs.ffmpeg-full}/bin/ffmpeg -y -f x11grab -video_size "1920x1080" -i "$DISPLAY" -filter_complex "boxblur=8:8" -frames:v 1 $TMPBG

          ${pkgs.i3lock}/bin/i3lock -i $TMPBG
        '';
      in "${ff-locker}/bin/ff-locker";
    };
  };
}
