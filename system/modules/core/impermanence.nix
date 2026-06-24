{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.system.modules.core.impermanence;
in
{
  options.system.modules.core.impermanence.enable = mkEnableOption "Erase your darling...";

  config = mkIf cfg.enable {
    environment.persistence."/persist" = {
      hideMounts = true;
      directories = [
        "/etc/NetworkManager/system-connections"
        "/var/lib/nixos"
        "/var/lib/NetworkManager"
        "/var/lib/bluetooth"
        "/var/lib/libvirt"
        "/var/lib/fprint"
        "/var/lib/docker"
        "/var/lib/AccountsService/icons"
        "/root/.ssh"
        "/root/.gnupg"

        # "/var/lib/cni"
        # "/var/lib/containers"
      ];
      files = [
        "/etc/adjtime"
        "/etc/machine-id"
        # No more needed as I don't use both Wayland and X11 sessions on the same device
        # "/var/lib/sddm/state.conf" # Name of the session for the last logged-in user
      ];
    };

    services.openssh.hostKeys = [
      {
        path = "/persist/etc/ssh/persist_ssh_key";
        type = "ed25519";
      }
    ];

    security.sudo.extraConfig = ''
      # Rollback results in sudo lectures after each reboot
      Defaults lecture = never
    '';
  };
}
