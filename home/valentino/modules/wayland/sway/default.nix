{
  config,
  lib,
  pkgs,
  outputs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.wayland;
  cfgTheme = config.valentino.modules.themes;
  mod = "Mod4";
  mod1 = "Mod1";
in {
  config = mkIf (cfg.enable && (elem "sway" cfg.wm)) {
    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      systemd.enable = true;

      config = let
        settings = import ./settings.nix {
          default_mod = "${mod}";
          alt_mod = "${mod1}";
          font = cfgTheme.font.regular;
          packages = pkgs;
        };
      in {
        bars = [{command = "waybar";}];
        modifier = "${mod}";
        floating = {
          modifier = "${mod}";
          border = 1;
          criteria = [
            {
              app_id = "pavucontrol";
              command = "resize set 480 480";
            }
          ];
        };
        gaps = {
          inner = 5;
          outer = 5;
          smartgaps = true;
          smartBorders = "on";
        };

        fonts = {
          names = [cfgTheme.font.regular.family];
          # Sum required: floating point value but int option defined
          size = cfgTheme.font.regular.size + 0.0;
        };

        # inherit (settings) menu;

        "bindgesture swipe:3:right" = "workspace back_and_forth";
        "bindgesture swipe:3:left" = "workspace back_and_forth";
        "bindgesture swipe:3:up" = "fullscreen";
        workspaceAutoBackAndForth = true;
        inherit (settings) workspaceOutputAssign;
        inherit (settings) startup;
        inherit (settings) keybindings;
      };

      # config = {

      #   input."type:keyboard" = {
      #     xkb_layout = "us";
      #     xkb_variant = "altgr-intl";
      #     xkb_options = "ctrl:nocaps";
      #   };

      #   input."1390:268:ELECOM_TrackBall_Mouse_HUGE_TrackBall" = {
      #     scroll_method = "on_button_down";
      #     scroll_button = "BTN_TASK";
      #   };

      #   output."Unknown U34G2G4R3 0x0000241D" = {
      #     mode = "3440x1440@144.001Hz";
      #   };

      #   # output."*" = {bg = "${outputs.wallpapers.city-lights.src} fill";};

      #   focus.followMouse = true;

      #   window = {
      #     titlebar = false;
      #     border = 4;

      #     commands = [
      #       {
      #         command = "floating enable";
      #         criteria = {app_id = "thunar";};
      #       }
      #       {
      #         command = "floating enable";
      #         criteria = {app_id = "ipv";};
      #       }
      #       {
      #         command = "floating enable";
      #         criteria = {app_id = "mpv";};
      #       }
      #       {
      #         command = "floating enable position center, focus";
      #         criteria = {app_id = "GtkFileChooserDialog";};
      #       }
      #       {
      #         command = "floating enable position center, focus";
      #         criteria = {app_id = "pop-up";};
      #       }
      #       {
      #         command = "floating enable position center, focus";
      #         criteria = {app_id = "Organizer";};
      #       }
      #       {
      #         command = "floating enable position center, focus";
      #         criteria = {app_id = "task_dialog";};
      #       }
      #       {
      #         command = "floating enable position center, focus";
      #         criteria = {title = "^Polychromatic$";};
      #       }
      #       {
      #         command = "floating enable";
      #         criteria = {app_id = "pavucontrol";};
      #       }
      #       {
      #         command = "floating enable, sticky enable";
      #         criteria = {
      #           app_id = "firefox";
      #           title = "^Picture-in-Picture$";
      #         };
      #       }
      #       {
      #         command = "floating enable, sticky enable, border none, nofocus";
      #         criteria = {title = " — Sharing Indicator$";};
      #       }
      #       {
      #         command = "shortcuts_inhibitor disable";
      #         criteria = {app_id = "^chrome-.*";};
      #       }
      #     ];
      #   };

      #   floating = {
      #     modifier = "Mod4";
      #     border = 3;
      #   };
      # };
      extraConfig = ''
'';
    };

    home.packages = with pkgs; [autotiling];

    valentino.modules = {
      wayland = {
        locker.enable = true;
        waybar.enable = true;
      };
    };
  };
}
