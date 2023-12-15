{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.files;
in {
  options.valentino.modules.media.files = {
    filezilla.enable = mkEnableOption "enable qrcp";
    qrcp = {
      enable = mkEnableOption "enable qrcp";
      interface = mkOption {
        type = types.str;
        default = "";
        description = ''Interface to use'';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.qrcp.enable {
      home.packages = with pkgs; [
        qrcp
      ];

      home.file.".config/qrcp/config.json" = {
        enable = true;
        text = ''
          {
              "fqdn": "",
              "interface": "${cfg.qrcp.interface}",
              "port": 9090,
              "keepAlive": false,
              "path": "",
              "secure": false,
              "tls-key": "",
              "tls-cert": "",
              "output": ""
          }
        '';
      };
    })

    (mkIf cfg.filezilla.enable {
      home.packages = with pkgs; [
        filezilla
      ];
    })
  ];
}
