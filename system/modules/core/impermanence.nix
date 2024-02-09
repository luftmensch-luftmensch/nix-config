{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.core.impermanence;
in {
  options.system.modules.core.impermanence = {
    enable = mkEnableOption "Erase your darling...";
  };

  config = mkIf cfg.enable {
    environment.persistence."/persist" = {
      hideMounts = true;
      directories = [
        "/etc/NetworkManager/system-connections"
        "/var/lib/bluetooth"
        # "/var/lib/cni"
        # "/var/lib/containers"
        "/var/lib/libvirt"
        "/var/lib/fprint"
        "/var/lib/docker"
        "/root/.ssh"
        "/root/.gnupg"
      ];
      files = [
        "/etc/adjtime"
        "/etc/machine-id"
        "/var/lib/NetworkManager/secret_key"
        "/var/lib/NetworkManager/seen-bssids"
        "/var/lib/NetworkManager/timestamps"
        "/var/lib/sddm/state.conf" # Name of the session for the last logged-in user
      ];
    };
    services = {
      openssh = {
        hostKeys = [
          {
            path = "/persist/etc/ssh/persist_ssh_key";
            type = "ed25519";
          }
        ];
      };
    };

    security.sudo.extraConfig = ''
      # Rollback results in sudo lectures after each reboot
      Defaults lecture = never
    '';
  };
}
