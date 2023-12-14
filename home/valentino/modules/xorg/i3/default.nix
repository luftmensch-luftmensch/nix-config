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
        config = {
          modifier = "${mod}";
          floating.modifier = "${mod}";
          bars = []; # use polybar instead
          gaps = {
            inner = 5;
            outer = 5;
            smartGaps = true;
            # smartBorders = "off";
          };

          # keybindings = (import ./keybindings.nix "${default_modifier}" "${alt_modifier}") {
          #   inherit pkgs;
          # };

          keybindings = {

            "F1" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10% && echo $(pamixer --get-volume) > $xob_sock";
            "F2" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10% && echo $(pamixer --get-volume) > $xob_sock";

            "F3" = "exec --no-startup-id ~/.local/bin/audio -a";
            "F4" = "exec --no-startup-id ~/.local/bin/audio -m";

            "F7" = "exec ~/.local/bin/playerctl-handler -p"; # Prev
            "F8" = "exec ~/.local/bin/playerctl-handler -x"; # Play/Pause
            "F9" = "exec ~/.local/bin/playerctl-handler -n"; # Next

            "${mod}+q" = "kill";
            "--release ${mod}+Escape" = "exec xkill";
            "${mod}+Shift+r" = "restart";

            # Focus
            "${mod}+Left" = "focus left";
            "${mod}+Down" = "focus down";
            "${mod}+Up" = "focus up";
            "${mod}+Right" = "focus right";
            "${mod}+h" = "focus left";
            "${mod}+j" = "focus down";
            "${mod}+k" = "focus up";
            "${mod}+l" = "focus right";

            # Move
            "${mod}+Shift+Left" = "move left";
            "${mod}+Shift+Down" = "move down";
            "${mod}+Shift+Up" = "move up";
            "${mod}+Shift+Right" = "move right";
            "${mod}+Shift+h" = "move left";
            "${mod}+Shift+j" = "move down";
            "${mod}+Shift+k" = "move up";
            "${mod}+Shift+l" = "move right";

            # Splitting
            "${mod}+z" = "split v; exec dunstify -h string:x-dunst-stack-tag:tile -t 600 -u low  'Tile horizontally'";
            "${mod}+v" = "split h; exec dunstify -h string:x-dunst-stack-tag:tile -t 600 -u low  'Tile vertically'";

            "${mod}+f" = "fullscreen toggle";

            # toggle tiling / floating
            "${mod}+Shift+space" = "floating toggle";
            # change focus between tiling / floating windows
            "${mod}+space" = "focus mode_toggle";

            "${mod}+x" = "exec rofi-powermenu";

            # $mod+x exec --no-startup-id "$menu"

            # switch to workspace
            "${mod}+1" = "workspace number 1";
            "${mod}+2" = "workspace number 2";
            "${mod}+3" = "workspace number 3";
            "${mod}+4" = "workspace number 4";
            "${mod}+5" = "workspace number 5";
            "${mod}+6" = "workspace number 6";
            "${mod}+7" = "workspace number 7";
            "${mod}+8" = "workspace number 8";
            "${mod}+9" = "workspace number 9";
            "${mod}+0" = "workspace number 10";

            # move focused container to workspace
            "${mod}+Shift+1" = "move container to workspace number 1";
            "${mod}+Shift+2" = "move container to workspace number 2";
            "${mod}+Shift+3" = "move container to workspace number 3";
            "${mod}+Shift+4" = "move container to workspace number 4";
            "${mod}+Shift+5" = "move container to workspace number 5";
            "${mod}+Shift+6" = "move container to workspace number 6";
            "${mod}+Shift+7" = "move container to workspace number 7";
            "${mod}+Shift+8" = "move container to workspace number 8";
            "${mod}+Shift+9" = "move container to workspace number 9";
            "${mod}+Shift+0" = "move container to workspace number 10";

            "${mod1}+Ctrl+Right" = "workspace next";
            "${mod1}+Ctrl+Left" = "workspace prev";

            "${mod}+Tab" = "workspace back_and_forth";
            "${mod}+Shift+Tab" = "workspace prev";

            # Start mode
            "${mod}+r" = "mode resize; exec dunstify -h string:x-dunst-stack-tag:volume -t 1000 -u low \"Resize\"";

            "Print" = "exec --no-startup-id xfce4-screenshooter -r";
            "${mod}+Return" = "exec --no-startup-id ${pkgs.alacritty}/bin/alacritty -t Alacritty -e fish";
            "${mod}+Shift+Return" = "exec --no-startup-id ${pkgs.alacritty}/bin/alacritty -t floating_term -e fish";

            "${mod}+b" = "exec --no-startup-id ${pkgs.firefox}/bin/firefox";
            "${mod}+d" = "exec --no-startup-id ${pkgs.dmenu}/bin/dmenu_run -nb '#0F0F0F' -nf '#c5c8c6' -sb '#3B4252' -sf '#c5c8c6' -fn 'Sarasa Mono Slab SC:size=10' -p 'Run: '";

            "${mod}+e" = "exec --no-startup-id ${pkgs.cinnamon.nemo}/bin/nemo";

            "${mod}+m" = "exec --no-startup-id ${pkgs.emacs}/bin/emacsclient -c";
            "${mod}+o" = "exec --no-startup-id ${pkgs.obs-studio}/bin/obs";
            "${mod}+p" = "exec --no-startup-id ${pkgs.pavucontrol}/bin/pavucontrol";

            "${mod}+Shift+b" = "exec --no-startup-id ${pkgs.chromium}/bin/chromium";
            "${mod}+Shift+c" = "exec --no-startup-id ${pkgs.vscodium}/bin/codium";
            "${mod}+Shift+i" = "exec --no-startup-id ${pkgs.jetbrains.idea-community}/bin/idea-community";
            "${mod}+Shift+p" = "exec --no-startup-id ~/.local/bin/screenshot";
            "${mod}+Shift+v" = "exec --no-startup-id ~/.local/bin/copy-to-clipboard";
            "${mod}+Shift+s" = "exec --no-startup-id ${pkgs.spotify}/bin/spotify";
            
          };
          # keybindings = import ./keybindings.nix mod;

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

          startup = [
            {
              command = "autotiling";
              always = true;
            }

            {
              command = "parcellite";
            }

            {
              command = "nm-applet";
            }
            {
              command = "rm -f $xob_sock && mkfifo $xob_sock && tail -f $xob_sock | xob -t 700";
            }
						{ command = "systemctl --user restart polybar"; always = true; notification = false; }
						# { command = "polybar --reload main &"; always = true; notification = false; }
						{ command = "emacs --fg-daemon"; always = true; notification = false; }
          ];
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
