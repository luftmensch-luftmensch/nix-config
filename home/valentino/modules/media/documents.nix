{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.documents;
  inherit (config.valentino.modules) themes;
  inherit (config.colorScheme) palette;
in {
  options.valentino.modules.media.documents = {
    zathura.enable = mkEnableOption "pdf support (zathura)";
    okular.enable = mkEnableOption "pdf support (okular)";
  };

  config = mkMerge [
    (mkIf cfg.zathura.enable {
      programs.zathura = {
        enable = true;
        options = {
          font = "${themes.font.term.family} " + (toString themes.font.term.size);

          default-bg = "#${palette.base00}";
          default-fg = "#${palette.base01}";

          statusbar-fg = "#${palette.base04}";
          statusbar-bg = "#${palette.base02}";

          inputbar-bg = "#${palette.base00}";
          inputbar-fg = "#${palette.base07}";

          notification-bg = "#${palette.base00}";
          notification-fg = "#${palette.base07}";

          notification-error-bg = "#${palette.base00}";
          notification-error-fg = "#${palette.base06}";

          notification-warning-bg = "#${palette.base00}";
          notification-warning-fg = "#${palette.base06}";

          highlight-color = "#${palette.base0A}";
          highlight-active-color = "#${palette.base0D}";

          completion-bg = "#${palette.base01}";
          completion-fg = "#${palette.base0D}";

          recolor-lightcolor = "#${palette.base00}";
          recolor-darkcolor = "#${palette.base06}";

          recolor = "true";
          recolor-keephue = "false";
          incremental-search = true;
          adjust-open = "best-fit";
          pages-per-row = 1;
          show-directories = true;
          show-hidden = true;
          show-recent = 10;
          selection-clipboard = "clipboard";
          statusbar-home-tilde = true;
          guioptions = "sv";
        };
        mappings = {
          "<C-+>" = "zoom in";
          "<C-->" = "zoom out";
          "t" = "toggle_statusbar";
          "r" = "reload";
          "p" = "print";
          "i" = "recolor";
          "R" = "rotate";
          "f" = "toggle_fullscreen";
          "[fullscreen] f" = "toggle_fullscreen";
          "[fullscreen] d" = "toggle_page_mode 2";
          "[fullscreen] t" = "toggle_statusbar";
          # Available at ~/.local/share/zathura/bookmarks
          "bs" = "feedkeys \":bmark <Tab>\"";
          "bd" = "feedkeys \":bdelete <Tab>\"";
          "bl" = "feedkeys \":blist\"";
        };
      };

      home.packages = with pkgs; [
        pandoc # General markup converter
        poppler # A PDF rendering library
      ];
    })

    (mkIf cfg.okular.enable {home.packages = with pkgs; [okular];})
  ];
}
