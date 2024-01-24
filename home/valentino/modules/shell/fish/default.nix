{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.fish;
  cfgPolybar = config.valentino.modules.apps.polybar;
  cfgFoot = config.valentino.modules.term.foot;
  cfgWayland = config.valentino.modules.wayland;
in {
  options.valentino.modules.shell.fish = {
    enable = mkEnableOption "Fish - The friendly interactive shell";
    cpuTuning = mkEnableOption "enable cpu frequency tuning";
  };

  config = mkIf cfg.enable {
    programs.fish = {
      enable = true;
      shellInit = mkMerge [
        (import ./init.nix).shellInit

        (
          mkIf cfgFoot.enable
          ''
            # Taken from https://codeberg.org/dnkl/foot/wiki#user-content-spawning-new-terminal-instances-in-the-current-working-directory
                function update_cwd_osc --on-variable PWD --description 'Notify terminals when $PWD changes'
                    if status --is-command-substitution || set -q INSIDE_EMACS
                        return
                    end
                    printf \e\]7\;file://%s%s\e\\ $hostname (string escape --style=url $PWD)
                end

                update_cwd_osc # Run once since we might have inherited PWD from a parent shell
          ''
        )
      ];

      shellAbbrs = {
        pc = "valentino@192.168.1.171";
        home = "valentino@192.168.1.203";
        sc = "systemctl";
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
              ${pkgs.imagemagick}/bin/convert -size 48x48 xc:"$color" $image
              ${pkgs.libnotify}/bin/notify-send -h string:x-canonical-private-synchronous:sys-notify -u low -i "$image" "$color, copied to clipboard."
              [ -f "$image" ] && rm "$image"
                    end
            '';
          };
        })
        (mkIf config.programs.git.enable {
          git-purge-history = let
            _git = "${pkgs.git}/bin/git";
          in {
            body = ''
              set current_branch (${_git} rev-parse --abbrev-ref HEAD)
              ${_git} checkout --orphan temp
              ${_git} add -A
              ${_git} commit -am "¯\_(ツ)_/¯"
              ${_git} branch -D $current_branch
              ${_git} branch -m $current_branch
              ${_git} push -f origin main
            '';
          };
        })
      ];
    };
  };
}
