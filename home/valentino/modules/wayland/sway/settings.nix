{
  default_mod,
  alt_mod,
  theme,
  wallpaper_path,
  pkgs,
}: let
  audio_cmd = "${pkgs.wireplumber}/bin/wpctl";
  noti_cmd = "${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
  bright_cmd = "${pkgs.brightnessctl}/bin/brightnessctl set";

  # Custom scripts
  rws = pkgs.callPackage ./scripts/random-wallpaper.nix {
    inherit wallpaper_path;
  };
  bss = pkgs.callPackage ./scripts/battery-status.nix {};

  cms = pkgs.callPackage ./scripts/clipboard-manager.nix {
    inherit theme;
  };

  sus = pkgs.callPackage ./scripts/screenshot-utility.nix {
    inherit theme;
  };
in {
  # menu =
  keybindings = {
    # Recently wireplumber (v 0.4.11) added a few utilities for their wpctl tool. You can do:
    # set-volume and set-mute works with that helper, alternatively you can run wpctl status to get the ID.
    # Volume
    XF86AudioRaiseVolume = "exec --no-startup-id ${audio_cmd} set-volume @DEFAULT_SINK@ 10%+ && ${audio_cmd} get-volume @DEFAULT_SINK@ | awk '{print $2*100}' > $wob_sock";
    XF86AudioLowerVolume = "exec --no-startup-id ${audio_cmd} set-volume @DEFAULT_SINK@ 10%- && ${audio_cmd} get-volume @DEFAULT_SINK@ | awk '{print $2*100}' > $wob_sock";
    XF86AudioMute = "exec --no-startup-id ${audio_cmd} set-mute @DEFAULT_AUDIO_SINK@ toggle";

    # TODO: complete it
    XF86AudioMicMute = "exec --no-startup-id ${audio_cmd} set-mute @DEFAULT_AUDIO_SOURCE@ toggle"; # dunstify -h string:x-dunst-stack-tag:mute -t 800 -u low -a mic_unmuted "$(wpctl get-volume @DEFAULT_AUDIO_SOURCE@ | cut -d "[" -f2 | cut -d "]" -f1 | awk '{if ($1 == "MUTED") print " Mic Muted"; else print " Unmuted"}')"

    # BRIGHTNESS
    XF86MonBrightnessUp = "exec ${bright_cmd} 5%+ | ${pkgs.gnused}/bin/sed -En 's/.*\(([0-9]+)%\).*/\1/p' > $wob_sock";
    XF86MonBrightnessDown = "exec ${bright_cmd} 5%- | ${pkgs.gnused}/bin/sed -En 's/.*\(([0-9]+)%\).*/\1/p' > $wob_sock";

    # Notification
    XF86Messenger = "exec --no-startup-id ${noti_cmd}";
    "${default_mod}+x" = "exec --no-startup-id ${noti_cmd}";
    "${default_mod}+Shift+n" = "exec --no-startup-id ${noti_cmd}";

    "${default_mod}+q" = "kill";
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

    "${alt_mod}+Ctrl+h" = "exec --no-startup-id ${cms}/bin/cms";

    "${default_mod}+Tab" = "workspace back_and_forth";
    "${default_mod}+Shift+Tab" = "workspace prev";

    # Start mode
    "${default_mod}+r" = "mode resize; exec ${pkgs.libnotify}/bin/notify-send -t 1000 -u low \"Resize\"";

    Print = "exec --no-startup-id ${pkgs.grim}/bin/grim -g  \"$(${pkgs.slurp}/bin/slurp)\" $(date +'%d-%m-%Y-%H:%M:%S').png";

    "${default_mod}+Return" = "exec --no-startup-id ${pkgs.foot}/bin/foot -a=default_term -e fish";
    "${default_mod}+Shift+Return" = "exec --no-startup-id ${pkgs.foot}/bin/foot -a=floating_term -e fish";

    "${default_mod}+b" = "exec --no-startup-id ${pkgs.firefox}/bin/firefox";

    # TODO: Fix theming
    "${default_mod}+d" = "exec ${pkgs.bemenu}/bin/bemenu-run -i -p '▶ Run: ' --fn '${theme.font.regular.family}:size=${(toString theme.font.regular.size)}' --tb '#3B4252' --nb '#0F0F0F' --nf '#c5c8c6' --sb '#3B4252' --sf '#c5c8c6' --tf '#FFFFFF' --hf '#FFFFFF' --hb '#3B4252' | xargs swaymsg exec";

    "${default_mod}+e" = "exec --no-startup-id ${pkgs.cinnamon.nemo}/bin/nemo";

    "${default_mod}+m" = "exec --no-startup-id emacsclient -c";
    "${default_mod}+o" = "exec --no-startup-id ${pkgs.obs-studio}/bin/obs";
    "${default_mod}+p" = "exec --no-startup-id ${pkgs.pavucontrol}/bin/pavucontrol";

    "${default_mod}+Shift+b" = "exec --no-startup-id ${pkgs.chromium}/bin/chromium";
    "${default_mod}+Shift+c" = "exec --no-startup-id ${pkgs.vscodium}/bin/codium";
    "${default_mod}+Shift+i" = "exec --no-startup-id ${pkgs.jetbrains.idea-community}/bin/idea-community";
    "${default_mod}+Shift+s" = "exec --no-startup-id ${pkgs.spotify}/bin/spotify";

    "${default_mod}+Shift+p" = "exec --no-startup-id ${sus}/bin/sus";
  };

  workspaceOutputAssign = [
    # Laptop
    {
      workspace = "1";
      output = "$laptop";
    }

    {
      workspace = "3";
      output = "$laptop";
    }

    {
      workspace = "5";
      output = "$laptop";
    }

    {
      workspace = "7";
      output = "$laptop";
    }

    {
      workspace = "9";
      output = "$laptop";
    }

    # Monitor
    {
      workspace = "2";
      output = "$monitor";
    }

    {
      workspace = "4";
      output = "$monitor";
    }

    {
      workspace = "6";
      output = "$monitor";
    }

    {
      workspace = "8";
      output = "$monitor";
    }

    {
      workspace = "0";
      output = "$monitor";
    }
  ];

  startup = [
    {
      command = "corectrl";
    }

    {
      command = "autotiling";
      always = true;
    }

    {
      # Info about brightness & volume using wob
      command = "rm -f $wob_sock && mkfifo $wob_sock && tail -f $wob_sock | wob";
    }

    {
      command = "emacs --fg-daemon";
      always = true;
    }

    {
      command = "wl-paste --watch cliphist store";
    }

    {
      command = "swaync";
    }

    # Custom scripts
    {
      command = "${rws}/bin/rws";
      always = true;
    }

    {
      command = "${bss}/bin/bss";
    }
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
        criteria = {
          shell = "xwayland";
        };
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
        command = "floating enable, resize set 800 400";
        criteria = {
          app_id = "firefox";
          title = "^Lib$";
        };
      }

      {
        command = "floating enable, sticky enable, border none, nofocus";
        criteria = {
          title = " — Sharing Indicator$";
        };
      }

      {
        command = "floating enable, resize set 480 480";
        criteria = {
          app_id = "pavucontrol";
        };
      }

      {
        command = "floating enable, resize set 480 480";
        criteria = {
          app_id = "imv";
        };
      }

      {
        command = "floating enable, resize set 480 480, move right 300px, move down 50px, sticky enable";
        criteria = {
          app_id = "mpv";
        };
      }

      {
        command = "floating enable, resize set 800 480";
        criteria = {
          app_id = "floating_term";
        };
      }

      # Opacity rules
      {
        command = "opacity $opacity";
        criteria = {
          app_id = "floating_term";
        };
      }

      {
        command = "opacity $opacity";
        criteria = {
          app_id = "default_term";
        };
      }

      {
        command = "opacity $opacity";
        criteria = {
          app_id = "foot";
        };
      }

      {
        command = "opacity $opacity";
        criteria = {
          app_id = "Alacritty";
        };
      }

      {
        command = "opacity $opacity";
        criteria = {
          app_id = "Alacritty";
        };
      }

      {
        command = "opacity $opacity";
        criteria = {
          app_id = "emacs";
        };
      }

      # Inhibitors
      {
        # Stop Chrome from Stealing Sway's Hotkeys - https://artemis.sh/2022/09/15/stop-chrome-stealing-hotkeys-sway.html
        command = "shortcuts_inhibitor disable";
        criteria = {
          app_id = "^chrome-.*";
        };
      }

      {
        command = "inhibit_idle fullscreen";
        criteria = {
          # Match all
          shell = ".*";
        };
      }

      {
        command = "inhibit_idle fullscreen";
        criteria = {
          # Match all
          app_id = "chromium";
        };
      }
    ];
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
      "${default_mod}+r" = "mode default";
    };
  };

  input = {
    "*" = {
      xkb_layout = "it";
    };

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

    "2:14:ETPS/2_Elantech_TrackPoint" = {
      pointer_accel = "0.1";
    };
  };

  output = {
    "$laptop" = {
      res = "1920x1080  position 0,0";
    };

    "$monitor" = {
      res = "1920x1080  position 1920,0";
    };
  };
}
