{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg;
  mod = "Mod4";
  mod1 = "Mod1";
in {
  config = mkIf (cfg.enable && cfg.wm == "i3") {
    xsession = {
      enable = true;
      initExtra = "xset b off";
      windowManager.i3 = {
        enable = true;
        config = let
          settings = import ./settings.nix {
            default_mod = "${mod}";
            alt_mod = "${mod1}";
            packages = pkgs;
          };
        in {
          modifier = "${mod}";
          floating.modifier = "${mod}";
          bars = []; # use polybar instead
          gaps = {
            inner = 5;
            outer = 5;
            smartGaps = true;
            # smartBorders = "off";
          };

          keybindings = settings.keybindings;

          assigns = {
            "2" = [{class = "^obs";}];
          };

          modes = {
            resize = {
              "h" = "resize shrink width 10 px or 10 ppt";
              "j" = "resize grow height 10 px or 10 ppt";
              "k" = "resize shrink height 10 px or 10 ppt";
              "l" = "resize grow width 10 px or 10 ppt";

              # same bindings, but for the arrow keys
              "Left" = "resize shrink width 10 px or 10 ppt";
              "Down" = "resize grow height 10 px or 10 ppt";
              "Up" = "resize shrink height 10 px or 10 ppt";
              "Right" = "resize grow width 10 px or 10 ppt";

              # back to normal: Enter or Escape or $mod+r
              "Return" = "mode default";
              "Escape" = "mode default";
              "${mod}+r" = "mode default";
            };
          };

          startup = settings.startup;
        };

        extraConfig = ''
          set $xob_sock $XDG_RUNTIME_DIR/wob.sock
          # title_format "%title -- %class -- %instance" # (Enable it to select a correct for_window option)
          for_window [class=".*"] border pixel 1
          floating_minimum_size 75 x 50
          floating_maximum_size 1000 x 1000

          # Alacritty
          for_window [title="floating_term"] floating enable, move position center, resize set 700 400

          # Feh
          for_window [class="feh"] floating enable position center; focus
          for_window [instance="feh"] border none, move right 300px, move down 50px

          # Firefox windows
          for_window [window_role="Organizer"] floating enable position center; focus
          for_window [class="Organizer"] resize set 480 480
          for_window [class="Navigator"] floating enable position center; focus
          for_window [class="Navigator"] resize set 480 480
          for_window [class="Firefox" title="^Libreria"] floating enable
          for_window [class="Firefox" title="^Libreria"] resize set 800 400 for_window [class="Places"] floating enable position center; focus
          for_window [class="firefox" title="^Picture-in-Picture$"] floating enable, resize set 480px 270px, sticky enable

          for_window [window_role="Organizer"] floating enable position center; focus
          for_window [window_role="GtkFileChooserDialog"] floating enable position center; focus
          for_window [window_role="pop-up"] floating enable position center
          for_window [window_role="task_dialog"] floating enable position center
          for_window [class="Organizer"] resize set 480 480
          for_window [window_role="(?i)GtkFileChooserDialog"] floating enable, move absolute position 550 100

          # IMV
          for_window [class="imv"] floating enable, resize set 480 480, border none, move right 300px, move down 50px

          # Mpv
          for_window [class="mpv"] floating enable, sticky enable, resize set 480 480, move absolute position 1438 1

          # Pavucontrol
          for_window [class="Pavucontrol"] floating enable position center; focus
          for_window [class="Pavucontrol"] resize set 480 480

          # Teams
          for_window [title="Microsoft Teams Notification"] floating enable

          # Thunderbird
          for_window [window_role="Msgcompose" class="(?i)Thunderbird"] floating enable
        '';
      };
    };

    home.packages = with pkgs; [
      autotiling
      pamixer
    ];

    services = {
      pasystray.enable = true;
      # Random background using feh
      random-background = {
        enable = true;
        display = "scale";
        imageDirectory = "%h/Dropbox/Immagini/Anime";
        interval = "15min";
      };
    };

    valentino.modules = {
      apps = {
        dunst.enable = true;
        polybar = {
          enable = true;
          temperature = "/sys/class/thermal/thermal_zone2/temp";
        };
      };
      xorg.xob.enable = true;
    };
  };
}
