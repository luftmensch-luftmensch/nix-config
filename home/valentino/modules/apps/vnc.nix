{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.vnc;
in
{
  options.valentino.modules.apps.vnc = {
    remmina.enable = mkEnableOption "enable remmina";
    horizon.enable = mkEnableOption "enable vmware horizon client";
    anydesk.enable = mkEnableOption "enable anydesk";
    rustdesk.enable = mkEnableOption "enable rustdesk";
  };

  config = mkMerge [
    (mkIf cfg.remmina.enable { home.packages = [ pkgs.remmina ]; })
    (mkIf cfg.anydesk.enable { home.packages = [ pkgs.anydesk ]; })
    (mkIf cfg.rustdesk.enable { home.packages = [ pkgs.rustdesk ]; })
    (mkIf cfg.horizon.enable { home.packages = [ pkgs.vmware-horizon-client ]; })
  ];
}
