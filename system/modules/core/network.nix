{
  pkgs,
  lib,
  ...
}: let
  localRanges = [
    {
      from = 9000;
      to = 9091;
    }
  ];
in {
  networking = {
    firewall = {
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
  services = {
    # DNS
    resolved.enable = true;

    # VPN
    # tailscale.enable = true;
  };
  # Don't wait for network startup
  # https://old.reddit.com/r/NixOS/comments/vdz86j/how_to_remove_boot_dependency_on_network_for_a
  # systemd = {
  #   targets.network-online.wantedBy = pkgs.lib.mkForce []; # Normally ["multi-user.target"]
  #   services.NetworkManager-wait-online.wantedBy = pkgs.lib.mkForce []; # Normally ["network-online.target"]
  # };
}
