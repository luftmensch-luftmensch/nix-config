{
  default_mod,
  alt_mod,
	theme,
  pkgs,
  ...
}: let
  sus = pkgs.callPackage ./scripts/screenshot-utility.nix {
    inherit theme;
  };
in
{
  keybindings = {
    "F1" = "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -10% && echo $(${pkgs.pamixer}/bin/pamixer --get-volume) > $xob_sock";
    "F2" = "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +10% && echo $(${pkgs.pamixer}/bin/pamixer --get-volume) > $xob_sock";

    "F3" = "exec --no-startup-id ~/.local/bin/audio -a";
    "F4" = "exec --no-startup-id ~/.local/bin/audio -m";

    "F7" = "exec playerctl-wrapper -p"; # Prev
    "F8" = "exec playerctl-wrapper -x"; # Play/Pause
    "F9" = "exec playerctl-wrapper -n"; # Next

    "${default_mod}+q" = "kill";
    "--release ${default_mod}+Escape" = "exec xkill";
    "${default_mod}+Shift+r" = "restart";

    # Focus
    "${default_mod}+Left" = "focus left";
    "${default_mod}+Down" = "focus down";
    "${default_mod}+Up" = "focus up";
    "${default_mod}+Right" = "focus right";
    "${default_mod}+h" = "focus left";
    "${default_mod}+j" = "focus down";
    "${default_mod}+k" = "focus up";
    "${default_mod}+l" = "focus right";

    # Move
    "${default_mod}+Shift+Left" = "move left";
    "${default_mod}+Shift+Down" = "move down";
    "${default_mod}+Shift+Up" = "move up";
    "${default_mod}+Shift+Right" = "move right";
    "${default_mod}+Shift+h" = "move left";
    "${default_mod}+Shift+j" = "move down";
    "${default_mod}+Shift+k" = "move up";
    "${default_mod}+Shift+l" = "move right";

    # # Splitting
    "${default_mod}+z" = "split v; exec ${pkgs.libnotify}/bin/notify-send -t 600 -u low  'Tile horizontally'";
    "${default_mod}+v" = "split h; exec ${pkgs.libnotify}/bin/notify-send -t 600 -u low  'Tile vertically'";

    "${default_mod}+f" = "fullscreen toggle";

    # toggle tiling / floating
    "${default_mod}+Shift+space" = "floating toggle";
    # change focus between tiling / floating windows
    "${default_mod}+space" = "focus mode_toggle";

    "${default_mod}+x" = "exec rofi-powermenu";

    # $mod+x exec --no-startup-id "$menu"

    # switch to workspace
    "${default_mod}+1" = "workspace number 1";
    "${default_mod}+2" = "workspace number 2";
    "${default_mod}+3" = "workspace number 3";
    "${default_mod}+4" = "workspace number 4";
    "${default_mod}+5" = "workspace number 5";
    "${default_mod}+6" = "workspace number 6";
    "${default_mod}+7" = "workspace number 7";
    "${default_mod}+8" = "workspace number 8";
    "${default_mod}+9" = "workspace number 9";
    "${default_mod}+0" = "workspace number 10";

    # move focused container to workspace
    "${default_mod}+Shift+1" = "move container to workspace number 1";
    "${default_mod}+Shift+2" = "move container to workspace number 2";
    "${default_mod}+Shift+3" = "move container to workspace number 3";
    "${default_mod}+Shift+4" = "move container to workspace number 4";
    "${default_mod}+Shift+5" = "move container to workspace number 5";
    "${default_mod}+Shift+6" = "move container to workspace number 6";
    "${default_mod}+Shift+7" = "move container to workspace number 7";
    "${default_mod}+Shift+8" = "move container to workspace number 8";
    "${default_mod}+Shift+9" = "move container to workspace number 9";
    "${default_mod}+Shift+0" = "move container to workspace number 10";

    "${alt_mod}+Ctrl+Right" = "workspace next";
    "${alt_mod}+Ctrl+Left" = "workspace prev";

    "${default_mod}+Tab" = "workspace back_and_forth";
    "${default_mod}+Shift+Tab" = "workspace prev";

    # Start mode
    "${default_mod}+r" = "mode resize; exec ${pkgs.libnotify}/bin/notify-send -t 1000 -u low \"Resize\"";

    "Print" = "exec --no-startup-id ${pkgs.xfce.xfce4-screenshooter}/bin/xfce4-screenshooter -r";
    "${default_mod}+Return" = "exec --no-startup-id ${pkgs.alacritty}/bin/alacritty -t Alacritty -e fish";
    "${default_mod}+Shift+Return" = "exec --no-startup-id ${pkgs.alacritty}/bin/alacritty -t floating_term -e fish";

    "${default_mod}+b" = "exec --no-startup-id ${pkgs.firefox}/bin/firefox";
    "${default_mod}+d" = "exec --no-startup-id ${pkgs.dmenu}/bin/dmenu_run -nb '#0F0F0F' -nf '#c5c8c6' -sb '#3B4252' -sf '#c5c8c6' -fn '${theme.font.regular.family}:size=${(toString theme.font.regular.size)}' -p 'Run: '";

    "${default_mod}+e" = "exec --no-startup-id ${pkgs.cinnamon.nemo}/bin/nemo";

    "${default_mod}+m" = "exec --no-startup-id ${pkgs.emacs}/bin/emacsclient -c";
    "${default_mod}+o" = "exec --no-startup-id ${pkgs.obs-studio}/bin/obs";
    "${default_mod}+p" = "exec --no-startup-id ${pkgs.pavucontrol}/bin/pavucontrol";

    "${default_mod}+Shift+b" = "exec --no-startup-id ${pkgs.chromium}/bin/chromium";
    "${default_mod}+Shift+c" = "exec --no-startup-id ${pkgs.vscodium}/bin/codium";
    "${default_mod}+Shift+i" = "exec --no-startup-id ${pkgs.jetbrains.idea-community}/bin/idea-community";
		"${default_mod}+Shift+p" = "exec --no-startup-id ${sus}/bin/sus";
    "${default_mod}+Shift+v" = "exec --no-startup-id ~/.local/bin/copy-to-clipboard";
    "${default_mod}+Shift+s" = "exec --no-startup-id ${pkgs.spotify}/bin/spotify";
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
    {
      command = "systemctl --user restart polybar";
      always = true;
      notification = false;
    }
    # { command = "polybar --reload main &"; always = true; notification = false; }
    # {
    #   command = "emacs --fg-daemon";
    #   always = true;
    #   notification = false;
    # }
  ];
}
