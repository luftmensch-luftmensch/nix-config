{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.dunst;
  cfgWayland = config.valentino.modules.wayland;
  cfgTheme = config.valentino.modules.themes;
  inherit (config.colorScheme) colors;
in {
  options.valentino.modules.apps.dunst = {
    enable = mkEnableOption "dunst configuration";
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.libnotify
    ];

    services.dunst = {
      enable = true;

      settings = {
        global = {
          # Behavior
          follow = "mouse";
          fullscreen = "pushback";

          # Geometry
          width = "(300, 450)";
          height = "300";
          origin = "top-right";
          offset = let
            vMargin =
              if cfgWayland.enable
              then "10"
              else "40";
          in "10x${vMargin}";

          notification_limit = 3;

          # Progress bar
          progress_bar = true;
          progress_bar_height = 15;
          progress_bar_frame_width = 3;
          progress_bar_min_width = 300;
          progress_bar_max_width = 300;

          # Appearance
          indicate_hidden = true;
          transparency = 4;

          separator_height = 2;

          padding = 6;
          horizontal_padding = 6;
          text_icon_padding = 15;

          corner_radius = 5;
          frame_width = 2;
          frame_color = "#${colors.base0D}";
          separator_color = "frame";

          sort = "no";
          idle_threshold = 0;

          # Text
          font = let
            fontName = cfgTheme.font.monospace.family;
            fontSize = cfgTheme.font.monospace.size;
          in "${fontName} ${toString fontSize}, Material Design Icons ${
            toString fontSize
          }";

          line_height = 3;
          markup = "yes";

          format = "<b>%s</b>\\n%b";
          alignment = "center";
          vertical_alignment = "center";

          # Show age of message if message is older than show_age_threshold
          # seconds.
          # Set to -1 to disable.
          show_age_threshold = -1;
          word_wrap = true;
          # ellipsize = "middle";
          ignore_newline = "no";
          stack_duplicates = true;
          hide_duplicate_count = true;
          show_indicators = false;
          shrink = "no";

          # Icons
          enable_recursive_icon_lookup = true;
          icon_theme = "Adwaita";
          icon_position = "left";
          min_icon_size = 32;
          max_icon_size = 64;

          # History
          sticky_history = "yes";
          history_length = 15;

          # Misc
          title = "Dunst";
          class = "Dunst";

          dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst:";
          browser = "${pkgs.firefox}bin/firefox -new-tab";
          always_run_script = true;
          ignore_dbusclose = false;

          # Wayland
          force_xwayland = false;
          force_xinerama = false;

          # Mouse
          mouse_left_click = "do_action, close_current";
          mouse_middle_click = "close_all";
          mouse_right_click = "close_current";
        };

        urgency_low = {
          background = "#${colors.base00}";
          foreground = "#${colors.base05}";
          timeout = 5;
        };

        urgency_normal = {
          background = "#${colors.base00}";
          foreground = "#${colors.base05}";
          timeout = 5;
        };

        urgency_critical = {
          background = "#${colors.base0F}";
          foreground = "#${colors.base05}";
          timeout = 0;
        };

        experimental.per_monitor_dpi = false;
      };
    };
  };
}
