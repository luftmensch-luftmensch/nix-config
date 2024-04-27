{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.services.printing;
in {
  options.system.modules.services.printing = {
    cups.enable = mkEnableOption "Enable printing capabilities";
    sane.enable = mkEnableOption "Enable scanning capabilities";
  };

  config = mkMerge [
    (mkIf cfg.cups.enable {
      services.printing = {
        enable = true;
        drivers = [pkgs.brlaser];
      };
    })

    (mkIf cfg.sane.enable {
      hardware.sane = {
        enable = true;
        extraBackends = [pkgs.epkowa];
      };
    })
  ];
}
