{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.term.foot;
  cfgTheme = config.valentino.modules.themes;
  inherit (config.colorScheme) colors;
in {
  options.valentino.modules.term.foot = {
    enable = mkEnableOption "foot configuration";
  };

  config = mkIf cfg.enable {
    programs.foot = {
      enable = true;
      settings = {
        shell = "fish";
        term = "foot";
        app-id = "foot";
        title = "foot";
        locker-title = "no";
        font = let
          inherit (cfgTheme.font.term) family;
          inherit (cfgTheme.font.term) size;
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
        };

        dpi-aware = "no";
        pad = "0x0";
        notify = "notify-send -a ${app-id} -i ${app-id} ${title} ${body}";
        bell = {
        };

        scrollback = {
          lines = 1000;
        };

        url = {
          launch = "xdg-open ${url}";
        };

        cursor = {
          style = "beam";
          blink = "no";
          beam-thickness = 1;
        };

        mouse = {
          hide-when-typing = "yes";
        };

        colors = {
          background = "#${colors.base00}";
          foreground = "#${colors.base05}";
          regular0 = "#${colors.base00}";
          regular1 = "#${colors.base08}";
          regular2 = "#${colors.base0B}";
          regular3 = "#${colors.base09}";
          regular4 = "#${colors.base0D}";
          regular5 = "#${colors.base0E}";
          regular6 = "#${colors.base0C}";
          regular7 = "#${colors.base05}";

          bright0 = "#${colors.base03}";
          bright1 = "#${colors.base06}";
          bright2 = "#${colors.base0B}";
          bright3 = "#${colors.base09}";
          bright4 = "#${colors.base0D}";
          bright5 = "#${colors.base0E}";
          bright6 = "#${colors.base0C}";
          bright7 = "#${colors.base05}";
        };

        csd = {
        };

        key-bindings = {
          scrollback-up-page = "Page_Up";
          # scrollback-up-half-page=none
          # scrollback-up-line=none
          scrollback-down-page = "Page_Down";
          # scrollback-down-half-page=none
          # scrollback-down-line=none
          clipboard-copy = "Control+Shift+c";
          clipboard-paste = "Control+Shift+v";
          # primary-paste=Shift+Insert
          # search-start=Control+Shift+r

          font-increase = "Control+plus";
          font-decrease = "Control+minus Control+KP_Subtract";
          font-reset = "Control+0";

          spawn-terminal = "Control+Shift+n Control+Shift+t";
          # minimize=none
          # maximize=none
          # fullscreen=none
          # pipe-visible=[sh -c "xurls | fuzzel | xargs -r firefox"] none
          # pipe-scrollback=[sh -c "xurls | fuzzel | xargs -r firefox"] none
          # pipe-selected=[xargs -r firefox] none
          show-urls-launch = "Control+o";
          show-urls-copy = "Control+Shift+o";
        };

        search-bindings = {
          
        };

        url-bindings = {
          toggle-url-visible = "t";
        };

        mouse-bindings = {
        };
      };
    };
  };
}