{ config, lib, ... }:
with lib;
let
  cfg = config.valentino.modules.term.alacritty;
  inherit (config.valentino.modules) themes;
in
{
  options.valentino.modules.term.alacritty.enable = mkEnableOption "alacritty configuration";

  config = mkIf cfg.enable {
    stylix.targets.alacritty = {
      enable = true;
      fonts.enable = false;
      # opacity.enable = false;
    };

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
        };

        # Scrolling history and multiplier
        scrolling = {
          history = 1000;
          multiplier = 3;
        };

        # Font configuration
        font =
          let
            inherit (themes.font.term) family;
            inherit (themes.font.term) size;
          in
          {
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

        selection.save_to_clipboard = true;
        general.live_config_reload = true;
        cursor.style = {
          shape = "Beam";
          blinking = "off";
        };

        terminal.shell.program = "fish";

        mouse = {
          hide_when_typing = false;
          bindings = [
            {
              mouse = "Middle";
              action = "PasteSelection";
            }
          ];
        };

        keyboard.bindings = [
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
