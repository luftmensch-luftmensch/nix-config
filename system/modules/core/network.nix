_: {
  networking = {
    firewall = let
      localRanges = [
        {
          from = 9000;
          to = 9091;
        }
      ];
    in {
      enable = true;
      allowedTCPPortRanges = localRanges;
      allowedUDPPortRanges = localRanges;
      allowedUDPPorts = [51820];
    };

    nameservers = [
      # DNS recursive service w/ Quad9 on ipv4 & ipv6
      "9.9.9.9"
      "149.112.112.112"

      "2620:fe::fe"
      "2620:fe::9"
    ];

    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
      wifi.powersave = true;
    };
  };

  # DNS
  services.resolved.enable = true;
}
