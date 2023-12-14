{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg.locker;
  ff-locker = pkgs.writeShellScriptBin "ff-locker" ''
    #!/usr/bin/env bash
    TMPBG=/tmp/screen.png
    display=$(xrandr | grep ' connected ' | awk '{print$1}' | wc -l)

    [ -f "$TMPBG" ] && rm $TMPBG

    ${pkgs.ffmpeg}/bin/ffmpeg -f x11grab -video_size "1920x1080" -i "$DISPLAY" -filter_complex "boxblur=8:8" -vframes 1 $TMPBG

    ${pkgs.i3lock}/bin/i3lock -i $TMPBG

  '';
in {
  options.valentino.modules.xorg.locker = {
    enable = mkEnableOption "xorg screen locker";
  };

  config = mkIf cfg.enable {
    services.screen-locker = {
      enable = true;
      xautolock = {
        enable = true;
        extraOptions = [
          "-time 10"
          # "-locker ${ff-locker}"
        ];
      };
      lockCmd = "${ff-locker}";
    };
  };
}
