{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.media.documents;
  inherit (config.valentino.modules) themes;
in
{
  options.valentino.modules.media.documents.enable = mkEnableOption "zathura pdf viewer";

  config = mkIf cfg.enable {
    stylix.targets.zathura.enable = true;
    programs.zathura = {
      enable = true;
      options = {
        font = "${themes.font.term.family} " + (toString themes.font.term.size);
        recolor = true;
        recolor-keephue = false;
        incremental-search = true;
        adjust-open = "best-fit";
        pages-per-row = 1;
        show-directories = true;
        show-hidden = true;
        show-recent = 10;
        selection-clipboard = "clipboard";
        statusbar-home-tilde = true;
        guioptions = "sv";
        database = "sqlite";
      };

      mappings = {
        "<C-+>" = "zoom in";
        "<C-->" = "zoom out";
        t = "toggle_statusbar";
        r = "reload";
        p = "print";
        i = "recolor";
        R = "rotate";
        f = "toggle_fullscreen";
        "[fullscreen] f" = "toggle_fullscreen";
        "[fullscreen] d" = "toggle_page_mode 2";
        "[fullscreen] t" = "toggle_statusbar";
        # Available at ~/.local/share/zathura/bookmarks
        bs = "feedkeys \":bmark <Tab>\"";
        bd = "feedkeys \":bdelete <Tab>\"";
        bl = "feedkeys \":blist\"";
      };
    };

    home.packages = with pkgs; [
      pandoc
      poppler
    ];
  };
}
