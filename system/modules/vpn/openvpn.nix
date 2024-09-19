{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.system.modules.vpn.openvpn;
in
{
  options.system.modules.vpn.openvpn.enable = mkEnableOption "Enable openvpn capabilities";

  config = mkIf cfg.enable {
    services.openvpn = {
      restartAfterSleep = false;
    };
  };
}
