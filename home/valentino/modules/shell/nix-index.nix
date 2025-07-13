{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.shell.nix-index;
  inherit (config.valentino.modules.shell) bash fish;
in
{
  imports = [ inputs.nix-index-database.homeModules.nix-index ];
  options.valentino.modules.shell.nix-index = {
    enable = mkEnableOption "Enable nix-index";
    service = mkEnableOption "Enable nix-index systemd service";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      programs = {
        command-not-found.enable = false; # `command-not-found` relies on nix-channel. Enable and use `nix-index` instead.
        nix-index-database.comma.enable = true;

        nix-index = {
          enable = true;
          enableBashIntegration = bash.enable;
          enableFishIntegration = fish.enable;
        };
      };
    })

    (mkIf cfg.service {
      assertions = [
        {
          assertion = cfg.enable;
          message = "In order to use the systemd service the nix-index options must be set to true";
        }
      ];
      systemd.user.services.nix-index-database-sync = {
        Unit.Description = "fetch mic92/nix-index-database";
        Service = {
          Type = "oneshot";
          ExecStart = lib.getExe (
            pkgs.writeShellApplication {
              name = "fetch-nix-index-database";
              runtimeInputs = with pkgs; [
                wget
                coreutils
              ];
              text = ''
                mkdir -p ~/.cache/nix-index
                pushd ~/.cache/nix-index
                name="index-${pkgs.stdenv.system}"
                wget -N "https://github.com/Mic92/nix-index-database/releases/latest/download/$name"
                ln -sf "$name" "files"
                popd
              '';
            }
          );
          Restart = "on-failure";
          RestartSec = "5m";
        };
      };
      systemd.user.timers.nix-index-database-sync = {
        Unit.Description = "Automatic github:mic92/nix-index-database fetching";
        Timer = {
          OnBootSec = "10m";
          OnUnitActiveSec = "24h";
        };
        Install.WantedBy = [ "timers.target" ];
      };
    })
  ];
}
