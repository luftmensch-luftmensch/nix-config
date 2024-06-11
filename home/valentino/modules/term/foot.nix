{ config, lib, ... }:
with lib;
let
  cfg = config.valentino.modules.term.foot;
  theme = config.valentino.modules.themes;
  inherit (config.colorScheme) palette;
in
{
  options.valentino.modules.term.foot.enable = mkEnableOption "foot configuration";

  config = mkIf cfg.enable {
    programs.foot = {
      enable = true;
      settings = {
        main =
          let
            inherit (theme.font.bar) family size;
          in
          {
            shell = "fish";
            term = "foot";
            app-id = "foot";
            title = "foot";
            locked-title = "no";

            dpi-aware = "yes";
            pad = "0x0";
            notify = "notify-send -a \${app-id} -i \${app-id} \${title} \${body}";
            font = "${family}:${toString size}";
            font-bold = "${family}:${toString size}";
            font-italic = "${family}:${toString size}";
            font-bold-italic = "${family}:${toString size}";
          };

        bell = { };

        scrollback.lines = 1000;

        url.launch = "xdg-open \${url}";

        cursor = {
          style = "beam";
          blink = "no";
          beam-thickness = 1;
        };

        mouse.hide-when-typing = "yes";

        colors = {
          background = "${palette.base00}";
          foreground = "${palette.base05}";
          regular0 = "${palette.base00}";
          regular1 = "${palette.base08}";
          regular2 = "${palette.base0B}";
          regular3 = "${palette.base09}";
          regular4 = "${palette.base0D}";
          regular5 = "${palette.base0E}";
          regular6 = "${palette.base0C}";
          regular7 = "${palette.base05}";

          bright0 = "${palette.base03}";
          bright1 = "${palette.base06}";
          bright2 = "${palette.base0B}";
          bright3 = "${palette.base09}";
          bright4 = "${palette.base0D}";
          bright5 = "${palette.base0E}";
          bright6 = "${palette.base0C}";
          bright7 = "${palette.base05}";
        };

        csd = { };

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

        search-bindings = { };

        url-bindings.toggle-url-visible = "t";

        mouse-bindings = { };
      };
    };
  };
}
