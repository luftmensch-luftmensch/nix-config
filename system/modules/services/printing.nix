{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.services.printing;
in {
  options.system.modules.services.printing = {
    enableCups = mkEnableOption "Enable printing capabilities";
    enableSane = mkEnableOption "Enable scanning capabilities";
  };

  config = mkMerge [
    (mkIf cfg.enableCups {
      services.printing = {
        enable = true;
        drivers = [ pkgs.brlaser ];
      };
    })

    (mkIf cfg.enableSane {
      hardware = {
        sane = {
          enable = true;
          extraBackends = [ pkgs.epkowa ];
        };
      };
    })
  ];
}
