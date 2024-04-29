{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.term.alacritty;
  inherit (config.valentino.modules) themes;
  inherit (config.colorScheme) palette;
in {
  options.valentino.modules.term.alacritty.enable = mkEnableOption "alacritty configuration";

  config = mkIf cfg.enable {
    programs.alacritty = {
      enable = true;

      settings = {
        # Window section settings
        window = {
          dimensions = {
            columns = 0;
            lines = 0;
          };
          padding = {
            x = 0;
            y = 0;
          };
          decorations = "none";
          opacity = 0.95;
        };

        # Scrolling history and multiplier
        scrolling = {
          history = 1000;
          multiplier = 3;
        };

        draw_bold_text_with_bright_colors = true;

        # Font configuration
        font = let
          inherit (themes.font.term) family;
          inherit (themes.font.term) size;
        in {
          normal = {
            inherit family;
            style = "Regular";
          };
          bold = {
            inherit family;
            style = "Bold";
          };
          italic = {
            inherit family;
            style = "Italic";
          };
          inherit size;
          offset = {
            x = 0;
            y = 0;
          };
        };

        colors = {
          primary = {
            background = "#${palette.base00}";
            foreground = "#${palette.base05}";
          };
          normal = {
            black = "#${palette.base00}";
            red = "#${palette.base08}";
            green = "#${palette.base0B}";
            yellow = "#${palette.base09}";
            blue = "#${palette.base0D}";
            magenta = "#${palette.base0E}";
            cyan = "#${palette.base0C}";
            white = "#${palette.base05}";
          };
          bright = {
            black = "#${palette.base03}";
            red = "#${palette.base06}";
            green = "#${palette.base0B}";
            yellow = "#${palette.base09}";
            blue = "#${palette.base0D}";
            magenta = "#${palette.base0E}";
            cyan = "#${palette.base0C}";
            white = "#${palette.base05}";
          };
        };

        selection.save_to_clipboard = true;
        live_config_reload = true;
        cursor.style = {
          shape = "Beam";
          blinking = "off";
        };

        shell.program = "fish";

        mouse = {
          hide_when_typing = false;
          hints.launcher.program = "xdg-open";
        };

        mouse_bindings = [
          {
            mouse = "Middle";
            action = "PasteSelection";
          }
        ];

        key_bindings = [
          {
            key = "Paste";
            action = "Paste";
          }

          {
            key = "Copy";
            action = "Copy";
          }

          {
            key = "PageUp";
            mods = "Shift";
            mode = "~Alt";
            action = "ScrollPageUp";
          }

          {
            key = "PageDown";
            mods = "Shift";
            mode = "~Alt";
            action = "ScrollPageDown";
          }

          {
            key = "Home";
            mods = "Shift";
            mode = "~Alt";
            action = "ScrollToTop";
          }

          {
            key = "End";
            mods = "Shift";
            mode = "~Alt";
            action = "ScrollToBottom";
          }

          {
            key = "F";
            mods = "Control|Shift";
            action = "SearchForward";
          }

          {
            key = "N";
            mods = "Control|Shift";
            action = "SpawnNewInstance";
          }

          {
            key = "T";
            mods = "Control|Shift";
            action = "SpawnNewInstance";
          }
        ];
      };
    };
  };
}
