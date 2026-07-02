{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.system.modules.services.printing;
in
{
  options.system.modules.services.printing = {
    cups.enable = mkEnableOption "Enable printing capabilities";
    sane.enable = mkEnableOption "Enable scanning capabilities";
  };

  config = mkMerge [
    (mkIf cfg.cups.enable {
      services.printing = {
        enable = true;
        # TODO: Try out cups-brother-hl1210w -> Unfortunately adding this one triggers a complete rebuild of glibc
        drivers = [ pkgs.brlaser ];
      };

      hardware.printers = {

        ensureDefaultPrinter = "printer";
        ensurePrinters = [
          {
            name = "printer";
            location = "printer";
            deviceUri = "ipp://192.168.1.74";
            model = "drv:///brlaser.drv/br1210.ppd";
            ppdOptions = {
              PageSize = "A4";
            };
          }
        ];
      };

    })

    (mkIf cfg.sane.enable {
      hardware.sane.enable = true;
    })
  ];
}
