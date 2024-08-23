{
  temperature,
  palette,
  pkgs,
}:
{
  "module/tray" = {
    type = "internal/tray";
    format-margin = "8px";
    tray-spacing = "8px";
    # tray-size = 80;
    # tray-max-size = 80;
    tray-maxsize = 28;
    tray-padding = 5;
    tray-scale = 1.0;
  };
  "module/bctl" =
    let
      cmd = pkgs.callPackage ./scripts/bluetooth.nix { };
    in
    {
      type = "custom/script";
      exec = "${cmd}/bin/bluetooth-ctl";
      tail = true;
      format-foreground = "${palette.base0D}";
      # label-maxlen = 55;
    };

  "module/date" = {
    type = "internal/date";
    interval = "1.0";
    time = "%H:%M";
    format = "%{A1:${pkgs.xfce.orage}/bin/orage:}<label>%{A}";
    format-prefix = " ";
    format-prefix-font = 2;
    format-prefix-foreground = "#${palette.base08}";
    label = "%time%";
  };

  "module/volume" = {
    type = "internal/pulseaudio";
    use-ui-max = false;
    interval = 5;

    format-volume = "<ramp-volume>  <label-volume>";
    format-muted-prefix = "婢";
    format-muted-prefix-font = 2;
    format-muted-prefix-foreground = "${palette.base08}";

    label-volume = "%percentage%%";
    label-muted = "  Muted";

    ramp = {
      volume = [
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
      ];
      headphones = [
        ""
        ""
      ];
    };

    ramp-volume-font = 2;
    click-right = "${pkgs.pavucontrol}/bin/pavucontrol";
  };

  "module/cpu" = {
    type = "internal/cpu";
    interval = 1;
    format-prefix = " ";
    label-font = 1;
    format = "<label>";
    format-prefix-foreground = "${palette.base0B}";
    label = "Cpu %percentage:%%";
  };

  "module/memory" = {
    type = "internal/memory";
    interval = 1;
    format-prefix = "  ";
    format-prefix-foreground = "${palette.base0A}";
  };

  "module/temp" = {
    type = "internal/temperature";
    interval = 5;
    hwmon-path = "${temperature}";
    format = "<label>";
    format-padding = 2;
    format-font = 2;

    label = " %temperature-c%";
    label-warn = "   %temperature-c%";
    label-warn-foreground = "${palette.base08}";
    content-font = 2;
  };

  "module/i3" = {
    type = "internal/i3";
    workspace-label = "%index%";
    pin-workspaces = true;
    strip-wsnumbers = false;
    index-sort = false;
    enable-click = true;
    enable-scroll = true;
    wrapping-scroll = false;
    reverse-scroll = true;
    fuzzy-match = false;
    label-focused = "\${self.workspace-label}";
    label-unfocused = "\${self.workspace-label}";
    label-urgent = "\${self.workspace-label}";
    label-visible = "\${self.workspace-label}";

    label-focused-foreground = "#${palette.base0C}";
    label-urgent-foreground = "#${palette.base08}";
    label-separator = "  ";

    label-mode-foreground = "#${palette.base08}";
    label-mode-padding = 2;
    # label-separator-padding = 2;
  };

  "module/title" = {
    type = "internal/xwindow";
    format = "<label>";
    label = "%title%";
    label-maxlen = 35;
  };

  "module/notifications" =
    let
      notify = "${pkgs.dunst}/bin/dunstctl";
    in
    {
      type = "custom/script";
      tail = true;
      format-padding = 0;
      click-left = "${notify} set-paused toggle";
      click-right = "${notify} close-all";
      exec = "if [[ \$(${notify} is-paused) = false ]]; then echo ' '; else echo ' '; fi";
    };
}
