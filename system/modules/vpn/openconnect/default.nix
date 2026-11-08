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

  sopsFile = ./secrets/openconnect.yaml;
  unit = "openconnect-work";

  ph = field: config.sops.placeholder."openconnect/${field}";

  confFields = [
    "server"
    "protocol"
    "user"
    "authgroup"
    "servercert"
  ];
in
{
  options.system.modules.vpn.openconnect.enable = mkEnableOption "Enable openconnect capabilities";

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.openconnect
    ]
    ++ optionals networkmanager.enable [ pkgs.networkmanager-openconnect ];

    sops.secrets = genAttrs (map (f: "openconnect/${f}") (confFields ++ [ "password" ])) (_: {
      inherit sopsFile;
    });

    sops.templates."${unit}.conf".content = ''
      ${concatMapStringsSep "\n" (f: "${f}=${ph f}") confFields}
      passwd-on-stdin
      non-inter
      base-mtu=1340
    '';

    systemd.services.${unit} = {
      description = "Work VPN w/ openconnect";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.openconnect}/bin/openconnect --config ${
          config.sops.templates."${unit}.conf".path
        }";
        StandardInput = "file:${config.sops.secrets."openconnect/password".path}";
      };
    };
  };
}
