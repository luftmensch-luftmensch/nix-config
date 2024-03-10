{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.images;
  inherit (config.valentino.modules) wayland;
in {
  options.valentino.modules.media.images = {
    imv.enable = mkEnableOption "an option to view images";
    feh.enable = mkEnableOption "an option to view images";
  };

  config = mkMerge [
    (mkIf cfg.imv.enable {
      programs.imv = {
        enable = true;
        settings = {
          binds = {
            "q" = "quit";
            "<Left>" = "prev";
            "<bracketleft>" = "prev";
            "<Right>" = "next";
            "<bracketright>" = "next";
            "gg" = "goto 0";
            "<Shift+G>" = "goto -1";

            # Panning
            J = "pan 0 -50";
            K = "pan 0 50";
            H = "pan 50 0";
            L = "pan -50 0";

            # Zooming
            j = "zoom -1";
            k = "zoom 1";
            "<minus>" = "zoom -1";
            "<plus>" = "zoom 1";
            l = "next";
            h = "prev";

            "<Up>" = "zoom 1";
            "<Shift+plus>" = "zoom 1";
            i = "zoom 1";
            "<Down>" = "zoom -1";
            o = "zoom -1";

            # Other commands
            x = "close";
            f = "fullscreen";
            d = "overlay";
            p = "exec echo \$imv_current_file";
            c = "center";
            s = "scaling next";
            "<Shift+S>" = "upscaling next";
            a = "zoom actual";
            r = "reset";

            # Gif playback
            "<period>" = "next_frame";
            "<space>" = "toggle_playing";

            # Slideshow control
            t = "slideshow +1";
            "<Shift+T>" = "slideshow -1";
            "<Shift+W>" = let
              setWallpaperCmd =
                if wayland.enable
                then "swaymsg output \"*\" background ~/.cache/wallpaper fill"
                else "feh --bg-scale ~/.cache/wallpaper";
            in "exec cp -f \"\$imv_current_file\" ~/.cache/wallpaper && ${setWallpaperCmd}";
          };
        };
      };
    })

    (mkIf cfg.feh.enable {
      programs.feh = {
        enable = true;
        keybindings = {
          zoom_out = ["j" "minus"];
          zoom_in = ["k" "plus"];
          next_img = ["l" "Right"];
          prev_img = ["h" "Left"];
        };
      };
    })
  ];
}
