{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.core.environment;
in {
  options.system.modules.core.environment = {
    enable = mkEnableOption "Enable environment variables";
  };

  config = mkIf cfg.enable {
    environment = {
      variables = {
        QT_QTA_PLATFORMTHEME = "qt5ct";
        QT_QPA_PLATFORMTHEME = "qt5ct";
        #AMD_VULKAN_ICD = "RADV";
        EDITOR = "emacsclient -t";
        BROWSER = "firefox";
        PAGER = "less --quit-at-eof";
        GTK_THEME = "Adwaita:dark"; # "Materia-dark";
      };
      etc = {
        "xdg/user-dirs.defaults".text = ''
          XDG_DOWNLOAD_DIR="$HOME/Scaricati"
          XDG_DOCUMENTS_DIR="$HOME/Documenti"
          XDG_PICTURES_DIR="$HOME/Immagini"
          XDG_VIDEOS_DIR="$HOME/Video"
        '';

        "xdg/gtk-2.0/gtkrc".text = ''
          gtk-theme-name="Adwaita-dark"
          gtk-icon-theme-name="Adwaita"
          gtk-font-name="Sarasa Mono Slab SC 12"
          gtk-cursor-theme-name="capitaine-cursors"
          gtk-cursor-theme-size=0
          gtk-toolbar-style=GTK_TOOLBAR_BOTH_HORIZ
          gtk-toolbar-icon-size=GTK_ICON_SIZE_LARGE_TOOLBAR
          gtk-button-images=0
          gtk-menu-images=0
          gtk-enable-event-sounds=1
          gtk-enable-input-feedback-sounds=1
          gtk-xft-antialias=1
          gtk-xft-hinting=1
          gtk-xft-hintstyle="hintslight"
          gtk-xft-rgba="rgb"
        '';

        # "xdg/gtk-3.0/settings.ini".text = ''
        #   [Settings]
        #   gtk-theme-name=Materia-dark
        #   gtk-icon-theme-name=breeze-dark
        #   gtk-font-name=Sans 10
        #   gtk-cursor-theme-size=0
        #   gtk-toolbar-style=GTK_TOOLBAR_BOTH_HORIZ
        #   gtk-toolbar-icon-size=GTK_ICON_SIZE_LARGE_TOOLBAR
        #   gtk-button-images=0
        #   gtk-menu-images=0
        #   gtk-enable-event-sounds=1
        #   gtk-enable-input-feedback-sounds=1
        #   gtk-xft-antialias=1
        #   gtk-xft-hinting=1
        #   gtk-xft-hintstyle=hintslight
        #   gtk-xft-rgba=rgb
        #   gtk-cursor-theme-name=capitaine-cursors
        # '';
      };
    };
  };
}
