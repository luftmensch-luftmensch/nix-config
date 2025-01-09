{ lib, ... }:
{
  networking = {
    # The global dhcp has been deprecated upstream, therefore explicitly set to false here.
    # Use the new networkd service instead of the legacy "script-based" network setups.
    # Host may contain individual dhcp interfaces or systemd-networkd configurations in host
    # specific directories
    useDHCP = false;
    useNetworkd = lib.mkForce true;
    enableIPv6 = false;
    firewall =
      let
        localRanges = [
          {
            from = 9000;
            to = 9091;
          }
        ];
      in
      {
        enable = true;
        allowedTCPPortRanges = localRanges;
        allowedUDPPortRanges = localRanges;
        allowedUDPPorts = [ 51820 ];
      };

    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
      wifi.powersave = true;
      insertNameservers = [
        # DNS recursive service w/ Quad9 on ipv4 & ipv6
        "9.9.9.9"
        "149.112.112.112"

        "2620:fe::fe"
        "2620:fe::9"
      ];
    };
  };

  # DNS
  services.resolved = {
    enable = true;
    dnssec = "allow-downgrade";
    domains = [ "~." ];
    dnsovertls = "opportunistic";
  };
}
