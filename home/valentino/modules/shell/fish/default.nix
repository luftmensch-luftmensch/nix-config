{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.fish;
  cfgPolybar = config.valentino.modules.apps.polybar;
  cfgWayland = config.valentino.modules.wayland;
in {
  options.valentino.modules.shell.fish = {
    enable = mkEnableOption "Fish - The friendly interactive shell";
    cpuTuning = mkEnableOption "enable cpu frequency tuning";
  };

  config = mkIf cfg.enable {
    programs.fish = {
      enable = true;
      shellInit = (import ./init.nix).settings;

      shellAbbrs = {
        pc = "valentino@192.168.1.171";
        home = "valentino@192.168.1.203";
      };

      shellAliases = import ../aliases.nix pkgs;
      functions = mkMerge [
        (import ./functions.nix pkgs)

        (mkIf cfgPolybar.enable {
          reload-polybar = {
            body = ''
              echo "Reloading polybar..."
              ${pkgs.polybar}/bin/polybar-msg cmd restart > /dev/null 2>&1
            '';
          };
        })
        (mkIf cfg.cpuTuning {
          set-cpu = {
            body = ''
              switch "$argv[1]"
               case "max"
                   echo "Setting to max"
                   sudo ${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set -f 3.0Ghz > /dev/null 2>&1
               case '*'
                   sudo ${pkgs.linuxPackages.cpupower}/bin/cpupower frequency-set -f 2.0Ghz > /dev/null 2>&1
              end
            '';
          };
        })

        (mkIf cfgWayland.enable {
          color-picker = {
            body = ''
              set color (${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp -b 1B1F2800 -p)" -t ppm - | ${pkgs.imagemagick}/bin/convert - -format '%[pixel:p{0,0}]' txt:- | ${pkgs.coreutils}/bin/tail -n1 | ${pkgs.coreutils}/bin/cut -d' ' -f4)
							set image /tmp/color_picker_image.png

							if [ $color ];
								echo "$color" | tr -d "\n" | ${pkgs.wl-clipboard}/bin/wl-copy 
								# generate preview
								${pgs.imagemagick}/bin/convert -size 48x48 xc:"$color" $image
								# notify about it
								${pkgs.libnotify}/bin/notify-send -h string:x-canonical-private-synchronous:sys-notify -u low -i "$image" "$color, copied to clipboard."
								[ -f "$image" ] && rm "$image"
							end
            '';
          };
        })
      ];
    };
  };
}
