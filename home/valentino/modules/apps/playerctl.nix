{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.playerctl;
  notify_cmd = "${pkgs.libnotify}/bin/notify-send -u low -t 900 -h string:x-dunst-stack-tag:music";
  play_cmd = "${pkgs.playerctl}/bin/playerctl";

  playerctl-wrapper = pkgs.writeShellScriptBin "playerctl-wrapper" ''
    pause_or_resume() {
      if [[ "$(${play_cmd} status)" == "Paused" ]]; then
        ${play_cmd} play && ${notify_cmd} "$(${play_cmd} metadata --format "Now playing: {{ artist }} - {{ title }} [{{duration(position)}}-{{duration(mpris:length)}}]")"
      else
        ${play_cmd} pause && ${notify_cmd} "$(${play_cmd} metadata --format "Paused: {{ artist }} - {{ title }}  [{{duration(position)}}-{{duration(mpris:length)}}]")"
      fi
    }

    while getopts ":xnp" opt; do
      case "$opt" in
        x) pause_or_resume ;;
        n) ${play_cmd} next ;;
        p) ${play_cmd} previous ;;
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
