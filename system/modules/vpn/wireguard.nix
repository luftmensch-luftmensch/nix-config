{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.vpn.wireguard;
in {
  options.system.modules.vpn.wireguard = {
    enable = mkEnableOption "Enable wireguard capabilities";
  };

  config = mkIf cfg.enable {
    networking.wg-quick.interfaces = {
      # "wg0" is the network interface name. You can name the interface arbitrarily.
      wg0 = {
        address = ["10.8.0.3/24"];
        listenPort = 51820;
        dns = ["1.1.1.1"];

        # Path to the private key file.
        #
        # Note: The private key can also be included inline via the privateKey option,
        # but this makes the private key world-readable; thus, using privateKeyFile is
        # recommended.
        privateKeyFile = "/home/valentino/Dropbox/vpn/wireguard/thinkpad_privatekey";

        peers = [
          # For a client configuration, one peer entry for the server will be enough.

          # HOME
          {
            # Public key of the server (not a file path).
            publicKey = "od98flJk4uIwawgfHQagMePqnnsgvhV4w7sq9h3ZGWU=";

            # Pre shared key used by wg-easy
            presharedKey = "Bq+DpD2XnPlXd815ysg9PBak9lWTGN6WuSSV5qx3Yt0=";

            # Forward all the traffic via VPN.
            allowedIPs = ["0.0.0.0/0" "::/0"];
            # Or forward only particular subnets
            #allowedIPs = [ "10.100.0.1" "91.108.12.0/22" ];

            # Set this to the server IP and port.
            endpoint = "homelab.myddns.me:51820"; # ToDo: route to endpoint not automatically configured https://wiki.archlinux.org/index.php/WireGuard#Loop_routing https://discourse.nixos.org/t/solved-minimal-firewall-setup-for-wireguard-client/7577

            # Send keepalives every 25 seconds. Important to keep NAT tables alive.
            persistentKeepalive = 0;
          }
        ];

        autostart = false;
      };
    };

    environment.systemPackages = [
      pkgs.wireguard-tools # Tools for the WireGuard secure network tunnel (wg-quick...)
    ];
  };
}
