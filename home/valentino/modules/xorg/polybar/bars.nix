{
  monitor,
  theme,
  palette,
}:
{
  "global/wm" = {
    margin-bottom = 0;
    margin-top = 0;
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

  "bar/configuration" =
    let
      family = theme.font.bar.family;
      size = theme.font.bar.size;
    in
    {
      font-0 = "${family}:weight=medium:size=${toString size};3";
      font-1 = "${family}:weight=medium:size=${toString size};3";
      font-2 = "${family}:weight=medium:size=${toString size};3";
      font-3 = "${family}:weight=medium:size=${toString size};3";
      background = "${palette.base00}";
      foreground = "${palette.base06}";

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
      padding = 1;
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

  "bar/main" = {
    "inherit" = "bar/configuration";
    inherit monitor;

    modules-left = "i3 title";
    modules-right = "temp bctl volume cpu memory date notifications tray";
    pseudo-transparent = true;
  };
}
