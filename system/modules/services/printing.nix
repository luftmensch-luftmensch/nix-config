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
        # TODO: Try out cups-brother-hl1210w
        drivers = with pkgs; [brlaser cups-brother-hl1210w];
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
