{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.system.modules.vpn.openconnect;
  inherit (config.networking) networkmanager;
in
{
  options.system.modules.vpn.openconnect.enable = mkEnableOption "Enable openconnect capabilities";

  config = mkIf cfg.enable {
    networking.openconnect.interfaces = {
      rev = {
        user = "revads015";
        passwordFile = "/home/valentino/Dropbox/vpn/openconnect/privatekey";
        gateway = "vpn.revspa.it";
        protocol = "anyconnect";
        autoStart = false;
        extraOptions = {
          authgroup = "REV_VPN";
          servercert = "pin-sha256:LYEs62Rl30PN6jmMgTta8Wr3EO0mpHuxITle25/HL6I=";
        };
      };
    };

    environment.systemPackages = [
      pkgs.openconnect
    ] ++ lib.optionals networkmanager.enable [ pkgs.networkmanager-openconnect ];
  };
}
