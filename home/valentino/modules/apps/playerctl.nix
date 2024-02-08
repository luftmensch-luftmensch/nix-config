{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.playerctl;
  _notify = "${pkgs.libnotify}/bin/notify-send -u low -t 900 -h string:x-dunst-stack-tag:music";
  _handler = "${pkgs.playerctl}/bin/playerctl";

  playerctl-wrapper = pkgs.writeShellScriptBin "playerctl-wrapper" ''
    pause_or_resume() {
      if [[ "$(${_handler} status)" == "Paused" ]]; then
        ${_handler} play && ${_notify} "$(${_handler} metadata --format "Now playing: {{ artist }} - {{ title }} [{{duration(position)}}-{{duration(mpris:length)}}]")"
      else
        ${_handler} pause && ${_notify} "$(${_handler} metadata --format "Paused: {{ artist }} - {{ title }}  [{{duration(position)}}-{{duration(mpris:length)}}]")"
      fi
    }

    while getopts ":xnp" opt; do
      case "$opt" in
        x) pause_or_resume ;;
        n) ${_handler} next ;;
        p) ${_handler} previous ;;
        *) printf "basename $0 needs a valid flag in order to work" ;;
      esac
    done
  '';
in {
  options.valentino.modules.apps.playerctl = {
    enable = mkEnableOption "playerct";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      playerctl
      playerctl-wrapper
    ];
  };
}
