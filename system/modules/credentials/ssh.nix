{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.credentials.ssh;
  user = config.system.modules.core.user.username;
in {
  options.system.modules.credentials.ssh.enable = mkEnableOption "sane ssh configuration";

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      startWhenNeeded = true;
      openFirewall = true;
      ports = [6969];

      settings = {
        X11Forwarding = false;
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        AllowTcpForwarding = false;
        AllowAgentForwarding = false;
        AllowStreamLocalForwarding = false;
        AuthenticationMethods = "publickey";
        # KbdInteractiveAuthentication = false;
      };

      allowSFTP = true;

      hostKeys = [
        {
          path = "/home/${user}/.ssh/id_homelab";
          bits = 4096;
          type = "ed25519";
        }
      ];
    };
  };
}
