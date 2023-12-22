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
      ];
    };
  };
}
