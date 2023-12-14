{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.documents;
  cfgTheme = config.valentino.modules.themes;
  inherit (config.colorScheme) colors;
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
          font =
            "${cfgTheme.font.term.family} "
            + (toString cfgTheme.font.term.size);

          default-bg = "#${colors.base00}";
          default-fg = "#${colors.base01}";

          statusbar-fg = "#${colors.base04}";
          statusbar-bg = "#${colors.base02}";

          inputbar-bg = "#${colors.base00}";
          inputbar-fg = "#${colors.base07}";

          notification-bg = "#${colors.base00}";
          notification-fg = "#${colors.base07}";

          notification-error-bg = "#${colors.base00}";
          notification-error-fg = "#${colors.base06}";

          notification-warning-bg = "#${colors.base00}";
          notification-warning-fg = "#${colors.base06}";

          highlight-color = "#${colors.base0A}";
          highlight-active-color = "#${colors.base0D}";

          completion-bg = "#${colors.base01}";
          completion-fg = "#${colors.base0D}";

          recolor-lightcolor = "#${colors.base00}";
          recolor-darkcolor = "#${colors.base06}";

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
          "<c-+>" = "zoom in";
          "<c-->" = "zoom out";
          "t" = "toggle_statusbar";
          "r" = "reload";
          "p" = "print";
          "i" = "recolor";
          "R" = "rotate";
          "f" = "toggle_fullscreen";
          "[fullscreen] f" = "toggle_fullscreen";
          "[fullscreen] d" = "toggle_page_mode 2";
          "[fullscreen] t" = "toggle_statusbar";
        };
      };
    })

    (mkIf cfg.okular.enable {home.packages = with pkgs; [okular];})
  ];
}
