{
  mod,
  mod1,
  theme,
  default_output,
  external_output,
  palette,
  pkgs,
}:
let
  _wpctl = "${pkgs.wireplumber}/bin/wpctl";
  _notification-center = "${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
  _notify = "${pkgs.libnotify}/bin/notify-send -r 1 -u low -t 1000";
  _brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl set";

  menu_opts = "-i --fn '${theme.font.regular.family} ${(toString theme.font.regular.size)}' --nb '#${palette.base00}'  --tb '#${palette.base01}' --hb '#${palette.base02}' --tf '#${palette.base0D}' --hf '#${palette.base0D}'";

  # Custom scripts
  bss = pkgs.callPackage ./scripts/battery-status.nix { };
  cms = pkgs.callPackage ./scripts/clipboard-manager.nix { inherit menu_opts; };
  sus = pkgs.callPackage ./scripts/screenshot-utility.nix { inherit menu_opts; };
in
{
  gaps = {
    inner = 5;
    outer = 5;
    smartGaps = true;
    smartBorders = "on";
  };

  fonts = {
    names = [ theme.font.regular.family ];
    # Sum required: floating point value but int option defined
    size = theme.font.regular.size + 0.0;
  };

  keybindings = {
    # Recently wireplumber (v 0.4.11) added a few utilities for their wpctl tool. You can do:
    # set-volume and set-mute works with that helper, alternatively you can run wpctl status to get the ID.
    # Volume
    XF86AudioRaiseVolume = "exec --no-startup-id ${_wpctl} set-volume @DEFAULT_SINK@ 10%+ && ${_wpctl} get-volume @DEFAULT_SINK@ | awk '{print $2*100}' > $wob_sock";
    XF86AudioLowerVolume = "exec --no-startup-id ${_wpctl} set-volume @DEFAULT_SINK@ 10%- && ${_wpctl} get-volume @DEFAULT_SINK@ | awk '{print $2*100}' > $wob_sock";
    XF86AudioMute = "exec --no-startup-id ${_wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle";

    XF86AudioMicMute = "exec --no-startup-id ${_wpctl} set-mute @DEFAULT_AUDIO_SOURCE@ toggle && ${_notify} -i audio-input-microphone \"$(${_wpctl} get-volume @DEFAULT_AUDIO_SOURCE@ | grep -q MUTED && echo 'Mic Muted' || echo 'Mic Unmuted')\"";

    # BRIGHTNESS
    XF86MonBrightnessUp = "exec ${_brightnessctl} 5%+ | grep -oP '(?<=[(])[^%)]*' > $wob_sock";
    XF86MonBrightnessDown = "exec ${_brightnessctl} 5%- | grep -oP '(?<=[(])[^%)]*' > $wob_sock";

    # Notification
    XF86Messenger = "exec --no-startup-id ${_notification-center}";
    "${mod}+x" = "exec --no-startup-id ${_notification-center}";
    "${mod}+Shift+n" = "exec --no-startup-id ${_notification-center}";

    "${mod}+q" = "kill";
    "${mod}+Shift+r" = "reload";

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
    "${mod}+z" = "split v; exec ${_notify} -i computer 'Tile horizontally'";
    "${mod}+v" = "split h; exec ${_notify} -i computer 'Tile vertically'";

    "${mod}+f" = "fullscreen toggle";

    # toggle tiling / floating
    "${mod}+Shift+space" = "floating toggle";
    # change focus between tiling / floating windows
    "${mod}+space" = "focus mode_toggle";

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

    "${mod1}+Ctrl+h" = "exec --no-startup-id ${cms}/bin/cms";

    "${mod}+Tab" = "workspace back_and_forth";
    "${mod}+Shift+Tab" = "workspace prev";

    # Start mode
    "${mod}+r" = "mode resize; exec ${_notify} -i video-display \"Resize\"";

    Print = "exec --no-startup-id ${pkgs.grim}/bin/grim -g  \"$(${pkgs.slurp}/bin/slurp)\" $(date +'%d-%m-%Y-%H:%M:%S').png";

    "${mod}+Return" = "exec --no-startup-id ${pkgs.foot}/bin/foot -a=default_term -e fish";
    "${mod}+Shift+Return" = "exec --no-startup-id ${pkgs.foot}/bin/foot -a=floating_term -e fish";

    "${mod}+b" = "exec --no-startup-id ${pkgs.firefox}/bin/firefox";

    "${mod}+d" = "exec ${pkgs.bemenu}/bin/bemenu-run ${menu_opts} -p '▶ Run: ' | xargs swaymsg exec";

    "${mod}+e" = "exec --no-startup-id ${pkgs.cinnamon.nemo}/bin/nemo";

    "${mod}+m" = "exec --no-startup-id emacsclient -c";
    "${mod}+o" = "exec --no-startup-id ${pkgs.obs-studio}/bin/obs";
    "${mod}+p" = "exec --no-startup-id ${pkgs.pavucontrol}/bin/pavucontrol";

    "${mod}+Shift+b" = "exec --no-startup-id ${pkgs.chromium}/bin/chromium";
    "${mod}+Shift+c" = "exec --no-startup-id ${pkgs.vscodium}/bin/codium";
    "${mod}+Shift+i" = "exec --no-startup-id ${pkgs.jetbrains.idea-community}/bin/idea-community";
    "${mod}+Shift+s" = "exec --no-startup-id ${pkgs.spotify}/bin/spotify";

    "${mod}+Shift+p" = "exec --no-startup-id ${sus}/bin/sus";
    "${mod}+Shift+e" = "exec rofi -show emoji -modi emoji -theme $HOME/.config/rofi/themes/emoji";
  };

  workspaceOutputAssign = [
    # Laptop
    {
      workspace = "1";
      output = default_output;
    }

    {
      workspace = "3";
      output = default_output;
    }

    {
      workspace = "5";
      output = default_output;
    }

    {
      workspace = "7";
      output = default_output;
    }

    {
      workspace = "9";
      output = default_output;
    }

    # Monitor
    {
      workspace = "2";
      output = external_output;
    }

    {
      workspace = "4";
      output = external_output;
    }

    {
      workspace = "6";
      output = external_output;
    }

    {
      workspace = "8";
      output = external_output;
    }

    {
      workspace = "0";
      output = external_output;
    }
  ];

  ### Sway logging & jorunalctl###
  # If you'd like sway's output to be handled by journald (like a systemd service), systemd-cat can be used for this:
  # exec systemd-cat --identifier=sway sway
  # You can print the logs with: `journalctl --user --identifier sway (Adding --follow & --this-boot might be handy)
  # dbus-sway-environment
  startup = [
    { command = "corectrl"; }

    {
      command = "autotiling";
      always = true;
    }

    {
      # Info about brightness & volume using wob
      command = "rm -f $wob_sock && mkfifo $wob_sock && tail -f $wob_sock | wob";
    }

    { command = "wl-paste --watch cliphist store"; }

    # Custom scripts
    { command = "${bss}/bin/bss"; }
  ];

  window = {
    titlebar = false;
    border = 1;
    commands = [
      # Enable it to select a correct for_window option
      # {
      #   command = "title_format \"%title -- %class -- %instance\"";
      #   criteria = {
      #     app_id = ".*";
      #   };
      # }

      {
        command = "title_format \"%title <small>[XWayland]</small>\"";
        criteria.shell = "xwayland";
      }

      # FF related
      {
        command = "floating enable, sticky enable";
        criteria = {
          app_id = "firefox";
          title = "^Picture-in-Picture$";
        };
      }

      {
        command = "floating enable position center, resize set 480 480, focus";
        criteria.app_id = "Organizer";
      }

      {
        command = "floating enable, resize set 800 400";
        criteria = {
          app_id = "firefox";
          title = "^Libreria";
        };
      }

      {
        command = "floating enable, sticky enable, border none, nofocus";
        criteria.title = " — Sharing Indicator$";
      }

      {
        command = "floating enable, resize set 480 480";
        criteria.app_id = "pavucontrol";
      }

      {
        command = "floating enable, resize set 480 480";
        criteria.app_id = "imv";
      }

      {
        command = "floating enable, resize set 480 480, move right 300px, move down 50px, sticky enable";
        criteria.app_id = "mpv";
      }

      {
        command = "floating enable, resize set 800 480";
        criteria.app_id = "floating_term";
      }

      # Opacity rules
      {
        command = "opacity $opacity";
        criteria.app_id = "floating_term";
      }

      {
        command = "opacity $opacity";
        criteria.app_id = "default_term";
      }

      {
        command = "opacity $opacity";
        criteria.app_id = "foot";
      }

      {
        command = "opacity $opacity";
        criteria.app_id = "Alacritty";
      }

      {
        command = "opacity $opacity";
        criteria.app_id = "Alacritty";
      }

      {
        command = "opacity $opacity";
        criteria.app_id = "emacs";
      }

      # Inhibitors
      {
        # Stop Chrome from Stealing Sway's Hotkeys - https://artemis.sh/2022/09/15/stop-chrome-stealing-hotkeys-sway.html
        command = "shortcuts_inhibitor disable";
        criteria.app_id = "^chrome-.*";
      }

      {
        command = "inhibit_idle fullscreen";
        criteria.shell = ".*"; # Match all
      }

      {
        command = "inhibit_idle fullscreen";
        criteria.app_id = "chromium";
      }
    ];
  };

  modes = {
    resize = {
      h = "resize shrink width 10 px or 10 ppt";
      j = "resize grow height 10 px or 10 ppt";
      k = "resize shrink height 10 px or 10 ppt";
      l = "resize grow width 10 px or 10 ppt";

      # same bindings, but for the arrow keys
      Left = "resize shrink width 10 px or 10 ppt";
      Down = "resize grow height 10 px or 10 ppt";
      Up = "resize shrink height 10 px or 10 ppt";
      Right = "resize grow width 10 px or 10 ppt";

      # back to normal: Enter or Escape or $mod+r
      Return = "mode default";
      Escape = "mode default";
      "${mod}+r" = "mode default";
    };
  };

  input = {
    "*".xkb_layout = "it";

    "type:mouse" = {
      accel_profile = "adaptive";
      pointer_accel = "0.4";
    };

    "type:touchpad" = {
      accel_profile = "adaptive";
      middle_emulation = "disabled";
      tap = "enabled";
      pointer_accel = "0.1";
      natural_scroll = "enabled";
      dwt = "enabled";
      drag = "enabled";
      scroll_method = "two_finger";
    };
  };

  output = {
    default_output.res = "1920x1080  position 0,0";
    external_output.res = "1920x1080  position 1920,0";
  };
}
