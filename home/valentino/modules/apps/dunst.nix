{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.dunst;
  inherit (config.valentino.modules) themes;
  inherit (config.colorScheme) palette;
in
{
  options.valentino.modules.apps.dunst.enable = mkEnableOption "dunst configuration";

  config = mkIf cfg.enable {
    # A library that sends desktop notifications to a notification daemon (Gonna hel dunst!)
    home.packages = [ pkgs.libnotify ];

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
          offset = "10x40";

          notification_limit = 3;

          # Progress bar
          progress_bar = true;
          progress_bar_height = 15;
          progress_bar_frame_width = 3;
          progress_bar_min_width = 300;
          progress_bar_max_width = 300;

          # Appearance
          indicate_hidden = true;
          transparency = 5;

          separator_height = 2;

          padding = 6;
          horizontal_padding = 6;
          text_icon_padding = 15;

          corner_radius = 5;
          frame_width = 2;
          frame_color = "#${palette.base0D}";
          separator_color = "frame";

          sort = "no";
          idle_threshold = 0;

          font =
            let
              inherit (themes.font.monospace) family size;
            in
            "${family} ${toString size}, Material Design Icons ${toString size}";

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

          dmenu = "${pkgs.bemenu}/bin/bemenu-run -p dunst:";
          browser = "${lib.getExe pkgs.firefox} -new-tab";
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
          background = "#${palette.base00}";
          foreground = "#${palette.base05}";
          timeout = 5;
        };

        urgency_normal = {
          background = "#${palette.base00}";
          foreground = "#${palette.base05}";
          timeout = 5;
        };

        urgency_critical = {
          background = "#${palette.base0F}";
          foreground = "#${palette.base05}";
          timeout = 0;
        };

        experimental.per_monitor_dpi = false;
      };
    };
  };
}
