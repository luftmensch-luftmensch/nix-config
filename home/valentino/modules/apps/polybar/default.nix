{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.polybar;
  cfgTheme = config.valentino.modules.themes;
  openCalendar = "${pkgs.xfce.orage}/bin/orage";
  bluetoothScript = pkgs.callPackage ./scripts/bluetooth.nix {};
  inherit (config.colorScheme) colors;
in {
  options.valentino.modules.apps.polybar = {
    enable = mkEnableOption "polybar configuration";
    # monitor = with types; (oneOf (enum ["HDMI" "eDP1"]));
    temperature = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    services.polybar = {
      enable = true;
      package = pkgs.polybarFull;
      settings = {
        "global/wm" = {
          margin-bottom = 0;
          margin-top = 0;
        };

        "bar/configuration" = {
          font-0 = "${cfgTheme.font.bar.family}:weight=medium:size=${toString cfgTheme.font.bar.size};3";
          font-1 = "${cfgTheme.font.bar.family}:weight=medium:size=${toString cfgTheme.font.bar.size};3";
          font-2 = "${cfgTheme.font.bar.family}:weight=medium:size=${toString cfgTheme.font.bar.size};3";
          font-3 = "${cfgTheme.font.bar.family}:weight=medium:size=${toString cfgTheme.font.bar.size};3";
          background = "${colors.base00}";
          foreground = "${colors.base06}";

          monitor-strict = false;

          override-redirect = false;
          fixed-center = true;
          width = "100%";
          bottom = true;
          height = 30;

          offset-x = 0;
          offset-y = 0;

          radius-top = "0.0";
          radius-bottom = "0.0";

          line-size = 1;

          border-size = 0;
          # padding = 1;
          module-margin-left = 1;
          module-margin-right = 1;
          separator = "";
          spacing = 0;
          dim-value = "1.0";
          wm-name = "i3";
          locale = "";
          enable-ipc = true;
          dpi = 100;
        };

        settings = {
          screenchange-reload = false;
          # Compositing operators
          # @see: https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-operator-t
          compositing-background = "source";
          compositing-foreground = "over";
          compositing-overline = "over";
          compositing-underline = "over";
          compositing-border = "over";
        };

        "bar/main" = {
          "inherit" = "bar/configuration";
          # monitor = "${cfg.monitor}";
          monitor = "HDMI1";

          modules-left = "i3 title";
          # modules-right = "temp volume cpu memory bluetooth date notifications tray";
					modules-right = "temp bctl volume cpu memory date notifications tray";
          # modules-right = "mail temp bluetooth volume cpu memory date notifications tray";
          pseudo-transparent = true;
        };

        "module/tray" = {
          type = "internal/tray";
          format-margin = "8px";
          tray-spacing = "8px";
          tray-size = 80;
          tray-padding = 5;
        };
				"module/bctl" = {
					type = "custom/script";
					exec = "${bluetoothScript}/bin/bluetooth-ctl";
					tail = true;
					# click-left = "${bluetoothScript}/bin/bluetooth-ctl --toggle &";
					# label-maxlen = 55;
				};

        "module/date" = {
          type = "internal/date";
          interval = "1.0";
          time = "%H:%M";
          # format = %{A1:~/.config/polybar/bin/calendar.sh:}<label>%{A}
          format = "%{A1:${openCalendar}:}<label>%{A}";
          format-prefix = " ";
          format-prefix-font = 2;
          # format-prefix-foreground = ${color.RED}
          label = "%time%";
        };

        "module/volume" = {
          type = "internal/pulseaudio";
          use-ui-max = false;
          interval = 5;

          format-volume = "<ramp-volume>  <label-volume>";
          format-muted-prefix = "婢";
          format-muted-prefix-font = 2;

          label-volume = "%percentage%%";
          label-muted = "  Muted";

					ramp = {
						volume = [ "" "" "" "" "" "" "" "" "" "" ];
						headphones = [ "" "" ];
					};

          ramp-volume-font = 2;

          click-right = "pavucontrol";
        };

        "module/cpu" = {
          type = "internal/cpu";
          interval = 1;
          format-prefix = " ";
          label-font = 1;
          format = "<label>";
          label = "Cpu %percentage:%%";
        };

        "module/memory" = {
          type = "internal/memory";
          interval = 1;
          format-prefix = "  ";
        };

        "module/temp" = {
          type = "internal/temperature";
          interval = 5;
          hwmon-path = "${cfg.temperature}";

          format = "<label>";
          format-padding = 2;
          format-font = 2;

          label = " %temperature-c%";
          label-warn = "   %temperature-c% ";
          content-font = 2;
        };

        "module/i3" = {
          type = "internal/i3";
          pin-workspaces = true;
          strip-wsnumbers = false;
          index-sort = false;
          enable-click = true;
          enable-scroll = true;
          wrapping-scroll = false;
          reverse-scroll = true;
          fuzzy-match = false;

          label-focused = "%name%";
          label-focused-padding = 1;

          label-occupied = "%name%";
          label-occupied-padding = 1;

          label-urgent = "%name%";
          label-urgent-padding = 1;

          label-empty = "%name%";
          label-empty-padding = 1;

          label-separator = " ";
          label-separator-padding = 0;
        };

        "module/title" = {
          type = "internal/xwindow";
          format = "<label>";
          label = "%title%";
          label-maxlen = 35;
        };

        "module/notifications" = {
          type = "custom/script";
          tail = true;
          format-padding = 0;
          click-left  = "${pkgs.dunst}/bin/dunstctl set-paused toggle";
          click-right = "${pkgs.dunst}/bin/dunstctl close-all";
          exec =  "if [[ \$(${pkgs.dunst}/bin/dunstctl  is-paused) = false ]]; then echo ' '; else echo ' '; fi";
        };

      };

        # for m in $(polybar --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
        #     MONITOR=$m polybar --reload main &
        # done
      script = ''
        polybar --reload main  2>/home/valentino/polybar.log &
      '';
    };
  };
}
