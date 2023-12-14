{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg.picom;
in {
  options.valentino.modules.xorg.picom = {
    enable = mkEnableOption "picom configuration";
  };

  config = mkIf cfg.enable {
    services.picom = {
      enable = true;

      backend = "glx";
      vSync = true;
      # experimentalBackends = true;

      # fade = true;
      # fadeDelta = 4;
      # fadeSteps = [2.8e-2 3.0e-2];

      activeOpacity = 1.0;
      inactiveOpacity = 1.0;

      # inactiveDim = 0;

      settings = {
        # Unredirect all windows if a full-screen opaque window is detected, to
        # maximize performance for full-screen windows. Known to cause
        # flickering when redirecting/unredirecting windows.
        # unredir-if-possible = true;

				shadow = false;
				# shadow-radius = 7;
				# shadowOffsets = [(-7) (-7)];
				# shadowExclude = [
				# 	"name = 'Notification'"
				# 	"class_g = 'Conky'"
				# 	"class_g ?= 'Notify-osd'"
				# 	"class_g = 'Cairo-clock'"
				# 	"_GTK_FRAME_EXTENTS@:c"
				# ];

				frame-opacity = 0.7;
				inactive-opacity-override = false;

        opacityRules = [
          "90:class_g = 'floating_term'"
          "90:class_g = 'kitty'"
          "90:class_g = 'Alacritty'"
        ];

        # GLX backend: Avoid using stencil buffer, useful if you don't have a
        # stencil buffer. Might cause incorrect opacity when rendering
        # transparent content (but never practically happened) and may not work
        # with blur-background. My tests show a 15% performance boost.
        # Recommended.
        # glx-no-stencil = true;

        # Other
        mark-wmwin-focused = true;
        mark-ovredir-focused = false;
        # use-ewmh-active-win = true;

        detect-rounded-corners = true;
        detect-client-opacity = true;
        detect-transient = true;
        detect-client-leader = true;

        focus-exclude = [
          "class_g = 'Cairo-clock'"
          # "window_type = 'dock'"
          # "! name~=''"
          # "class_g = 'Dunst'"
          # "class_g = 'Bar'" # lemonbar
          # "class_g = 'slop'" # maim
        ];

        blur-size = 138;
        blur-background = true;
        blur-kern = "3x3box";

        blur-background-exclude = [
          "window_type = 'dock'"
          "window_type = 'desktop'"
          "_GTK_FRAME_EXTENTS@:c"
        ];

        wintypes = {
          tooltip = {
            fade = true;
            shadow = true;
            opacity = 0.75;
            focus = true;
            full-shadow = false;
          };
          dock = {
            shadow = false;
          };
          dnd = {
            shadow = false;
          };
          popup_menu = {
            shadow = false;
          };
          dropdown_menu = {
            shadow = false;
          };
        };

        log-level = "warn";
      };
    };
  };
}
