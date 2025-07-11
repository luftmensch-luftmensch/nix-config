{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.media.videos;
in
{
  options.valentino.modules.media.videos.enable = mkEnableOption "an option to watch videos";

  config = mkIf cfg.enable {
    programs.mpv = {
      enable = true;
      config = {
        hwdec = "auto";
        title = "\${filename}";
        osc = "no";
        border = "no";

        screenshot-format = "png";
        screenshot-png-compression = 8;
        screenshot-template = "~/%F (%P) %n";

        audio-file-auto = "fuzzy";
        audio-pitch-correction = "yes";
        volume-max = 115;
        save-position-on-quit = "yes";
      };

      bindings = {
        h = "seek -5";
        l = "seek 5";
        RIGHT = "seek 5";
        LEFT = "seek -5";
        "Shift+PGUP" = "seek 600";
        "Shift+PGDWN" = "seek -600";

        # Volume
        j = "add volume -2";
        k = "add volume 2";

        # Speed
        "[" = "multiply speed 1/1.1";
        "]" = "multiply speed 1.1";
        "{" = "multiply speed 0.5";
        "}" = "multiply speed 2.0";

        # Quitting
        q = "quit";
        Q = "quit-watch-later";

        ESC = "set fullscreen no";

        # Frame
        "." = "frame-step";
        "," = "frame-back-step";

        f = "cycle fullscreen";
        SPACE = "cycle pause";
        "Ctrl+w" = "ignore";
        "Ctrl+c" = "ignore";
        WHEEL_LEFT = "ignore";
        WHEEL_RIGHT = "ignore";

        # Screenshot
        s = "screenshot";
        S = "screenshot video";
        "Ctrl+s" = "screenshot window";
        "Alt+s" = "screenshot each-frame";
        DEL = "script-binding osc/visibility";
      };
      scripts =
        with pkgs;
        [
          mpv-m-x
          mpv-navigator
          mpv-visualizer
        ]
        ++ (with pkgs.mpvScripts; [
          mpris
          modernx-zydezu
          thumbfast
        ]);
    };

    home.packages = with pkgs; [
      kooha
      obs-studio
    ];
  };
}
