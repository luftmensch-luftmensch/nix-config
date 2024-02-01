{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.polybar;
  theme = config.valentino.modules.themes;
  bluetoothScript = pkgs.callPackage ./scripts/bluetooth.nix {};
  inherit (config.colorScheme) palette;
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
      settings = let
        barConfig = import ./bars.nix {
					inherit theme palette;
        };

        moduleConfig = import ./modules.nix {
          b-cmd = "${bluetoothScript}/bin/bluetooth-ctl";
          c-cmd = "${pkgs.xfce.orage}/bin/orage";
          n-cmd = "${pkgs.dunst}/bin/dunstctl";
          temp = "${cfg.temperature}";
          volume = "${pkgs.pavucontrol}/bin/pavucontrol";
					inherit palette;
        };
      in
        barConfig // moduleConfig;

      # for m in $(polybar --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
      #     MONITOR=$m polybar --reload main &
      # done
      script = ''
        polybar --reload main &
      '';
    };
  };
}
