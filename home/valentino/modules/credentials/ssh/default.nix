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
      settings."*" = {
        ForwardAgent = false;
        AddKeysToAgent = "no";
        Compression = false;
        ServerAliveInterval = 0;
        ServerAliveCountMax = 3;
        HashKnownHosts = false;
        UserKnownHostsFile = "~/.ssh/known_hosts";

        ControlMaster = "auto";
        ControlPath = "~/.ssh/sockets/%r@%n-%p";
        ControlPersist = "60m";
      };
      includes = [
        "personal/config"
        "work/config"
      ];
    };
  };
}
