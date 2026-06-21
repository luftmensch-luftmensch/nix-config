{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.themes;
  inherit (config.valentino.modules) xorg;
in
{
  imports = [
    inputs.stylix.homeModules.stylix
    ./modus.nix
  ];

  options.valentino.modules.themes = with types; {
    active = mkOption {
      type = nullOr (enum [ "modus" ]);
      default = null;
      description = ''
        Name of the theme to enable.
      '';
    };

    darkTheme = mkOption {
      type = bool;
      default = false;
      description = ''
        If available, set the variant of chosen theme to light/dark one.
      '';
    };

    cursor.size = mkOption {
      type = int;
      default = 12;
    };

    dpi = mkOption {
      type = int;
      default = 120;
    };
  };

  config = mkIf (cfg.active != null) (mkMerge [
    # Globally enabled, for every system if a theme is selected
    {
      stylix = {
        enable = true;
        autoEnable = true;
        fonts = {
          serif = {
            name = cfg.font.regular.family;
            package = cfg.font.regular.package;
          };
          sansSerif = {
            name = cfg.font.regular.family;
            package = cfg.font.regular.package;
          };
          monospace = {
            name = cfg.font.monospace.family;
            package = cfg.font.monospace.package;
          };
          sizes = {
            applications = cfg.font.regular.size;
            desktop = cfg.font.regular.size;
            popups = cfg.font.regular.size;
            terminal = cfg.font.term.size;
          };
        };
        targets.gtk = {
          enable = true;
          fonts.enable = true;
        };
        targets.gnome.fonts.enable = false;
        opacity.terminal = 0.9;
      };

      gtk = {
        # gtk4.theme = null;

        font = {
          name = cfg.font.regular.family;
          inherit (cfg.font.regular) size;
        };
      };

    }

    # QT
    {
      home = {
        # sessionVariables.QT_QPA_PLATFORMTHEME = "qt6ct";
        packages = with pkgs.kdePackages; [
          qtstyleplugin-kvantum
          breeze
          qt6ct
        ];
      };
      stylix.targets.qt.enable = true;
    }

    # Xorg
    (mkIf xorg.enable {
      # home.pointerCursor.x11.enable = true;
      xresources.properties = {
        # "Xcursor.theme" = config.gtk.cursorTheme.name;
        "Xft.dpi" = cfg.dpi;
      };
    })
  ]);
}
