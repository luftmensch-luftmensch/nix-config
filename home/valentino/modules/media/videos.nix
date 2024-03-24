{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.videos;
  # inherit (config.valentino.modules) themes;
in {
  options.valentino.modules.media.videos = {
    enable = mkEnableOption "an option to watch videos";
  };

  config = mkIf cfg.enable {
    programs.mpv = {
      enable = true;
      config = {
        hwdec = "auto";
        title = "\${filename}";
        osc = "no";
        border = "no";
        # osd-level = 1;
        # osd-duration = 2500;
        #
        # osd-font = "${themes.font.term.family}";
        # osd-font-size = themes.font.term.size;
        # osd-border-size = 2;

        screenshot-format = "png";
        screenshot-png-compression = 8;
        screenshot-template = "~/%F (%P) %n";

        audio-file-auto = "fuzzy";
        audio-pitch-correction = "yes";
        volume-max = 115;
        volume = 90;
        save-position-on-quit = "yes";
      };

      bindings = {
        h = "seek -10";
        l = "seek 10";
        RIGHT = "seek 5";
        LEFT = "seek -5";
        "Shift+PGUP" = "seek 600";
        "Shift+PGDWN" = "seek -600";

        # Volume
        j = "add volume -2";
        k = "add volume 2";
        WHEEL_UP = "add volume 2";
        WHEEL_DOWN = "add volume -2";

        # Speed
        "[" = "multiply speed 1/1.1";
        "]" = "multiply speed 1.1";
        "{" = "multiply speed 0.5";
        "}" = "multiply speed 2.0";

        # Quitting
        q = "quit";
        Q = "quit-watch-later";
        "ctrl+w" = "quit";

        ESC = "set fullscreen no";

        # Frame
        "." = "frame-step";
        "," = "frame-back-step";

        f = "cycle fullscreen";
        SPACE = "cycle pause";

        # Screenshot
        s = "screenshot";
        S = "screenshot video";
        "Ctrl+s" = "screenshot window";
        "Alt+s" = "screenshot each-frame";
        DEL = "script-binding osc/visibility";
      };
      scripts = with pkgs;
        [mpv-modern-x mpv-navigator mpv-visualizer]
        ++ (with pkgs.mpvScripts; [mpris thumbfast sponsorblock]);
    };

    home.packages = [pkgs.obs-studio];
  };
}
