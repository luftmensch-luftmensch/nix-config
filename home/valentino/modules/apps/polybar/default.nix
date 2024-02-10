{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.polybar;
  theme = config.valentino.modules.themes;
  inherit (config.colorScheme) palette;
in {
  options.valentino.modules.apps.polybar = {
    enable = mkEnableOption "polybar configuration";
    monitor = mkOption {
      type = types.nullOr types.str;
      default = "HDMI1";
    };
    temperature = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    services.polybar = {
      enable = true;
      package = pkgs.polybarFull;
      settings =
        import ./bars.nix {
          inherit (cfg) monitor;
          inherit theme palette;
        }
        // import ./modules.nix {
          inherit (cfg) temperature;
          inherit pkgs palette;
        };

      script = ''
        polybar --reload main &
      '';
    };
  };
}
