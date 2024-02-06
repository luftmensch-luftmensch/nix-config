{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.files;
in {
  options.valentino.modules.media.files = {
    filezilla.enable = mkEnableOption "enable filezilla";
    libreoffice.enable = mkEnableOption "enable libreoffice";
    localsend.enable = mkEnableOption "localsend - An open source cross-platform alternative to AirDrop";
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

    (mkIf cfg.libreoffice.enable {
      home.packages = with pkgs; [
        libreoffice
      ];
    })

    (mkIf cfg.localsend.enable {
      home.packages = with pkgs; [
        localsend
      ];
    })
  ];
}
