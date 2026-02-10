{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.credentials.ssh;
in
{
  options.valentino.modules.credentials.ssh.enable = mkEnableOption "ssh user configuration";

  config = mkIf cfg.enable {
    sops.secrets = {
      "personal/config" = {
        sopsFile = ./personal/config;
        mode = "0400";
        format = "binary";
        path = "${config.home.homeDirectory}/.ssh/personal/config";
      };

      "work/config" = {
        sopsFile = ./work/config;
        mode = "0400";
        format = "binary";
        path = "${config.home.homeDirectory}/.ssh/work/config";
      };
    };

    programs.ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks."*" = {
        forwardAgent = false;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";

        controlMaster = "auto";
        controlPath = "~/.ssh/sockets/%r@%h-%p";
        controlPersist = "60m";
      };
      includes = [
        "personal/config"
        "work/config"
      ];
    };
  };
}
