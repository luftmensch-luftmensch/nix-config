{
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.credentials.ssh;
in {
  options.system.modules.credentials.ssh = {
    enable = mkEnableOption "ssh and a secure configuration";
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      startWhenNeeded = true;
      openFirewall = true;
      ports = [ 6969 ];

      settings = {
        X11Forwarding = false;
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        # KbdInteractiveAuthentication = false;
      };

      allowSFTP = false;

      # extraConfig = ''
      #   AllowTcpForwarding no
      #   AllowAgentForwarding no
      #   AllowStreamLocalForwarding no
      #   AuthenticationMethods publickey
      # '';
    };
  };
}
