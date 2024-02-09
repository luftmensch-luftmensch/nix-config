{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.credentials.ssh;
in {
  options.valentino.modules.credentials.ssh = {
    enable = mkEnableOption "ssh user configuration";
  };

  config = mkIf cfg.enable {
    sops = {
      secrets = {
        "personal/config" = {
          sopsFile = ./personal/config;
          mode = "0400";
          format = "binary";
          path = "${config.home.homeDirectory}/.ssh/personal/config";
        };

        "work/config" = {
          sopsFile = ./work/config;
          format = "binary";
          mode = "0400";
          path = "${config.home.homeDirectory}/.ssh/work/config";
        };
      };
    };

    programs = {
      ssh = {
        enable = true;
        includes = [
          "personal/config"
          "work/config"
        ];
      };
    };
  };
}
