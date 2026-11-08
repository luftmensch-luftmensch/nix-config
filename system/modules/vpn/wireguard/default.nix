# system/modules/vpn/wireguard/default.nix
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.system.modules.vpn.wireguard;

  sopsFile = ./secrets/wireguard.yaml;

  # Interface-level fields, shared by every location of a group
  groupFields = [
    "private_key"
    "address"
    "dns"
    "allowed_ips"
  ];

  # group -> interface prefix, locations, extra peer fields
  groups = {
    external = {
      prefix = "ext";
      locations = [
        "ca"
        "de"
        "es"
        "fr"
        "uk"
        "us"
      ];
      peerExtra = [ ];
    };
    internal = {
      prefix = "int";
      locations = [ "homelab" ];
      peerExtra = [ "preshared_key" ];
    };
  };

  enabledGroups = filter (g: cfg.${g}.enable) (attrNames groups);

  # One entry per location of the enabled groups
  entries = concatMap (
    g:
    map (l: {
      inherit g l;
      extra = groups.${g}.peerExtra;
    }) groups.${g}.locations
  ) enabledGroups;

  # Interface names are limited to 15 characters
  ifName = e: "${groups.${e.g}.prefix}-${e.l}";

  ph = path: config.sops.placeholder.${path};
  secret = path: nameValuePair path { inherit sopsFile; };
in
{
  options.system.modules.vpn.wireguard = mapAttrs (g: _: {
    enable = mkEnableOption "Enable ${g} wireguard tunnels";
  }) groups;

  config = mkIf (enabledGroups != [ ]) {
    environment.systemPackages = [ pkgs.wireguard-tools ];

    assertions = map (e: {
      assertion = stringLength (ifName e) <= 15;
      message = "wireguard: interface name '${ifName e}' exceeds 15 characters.";
    }) entries;

    sops.secrets = listToAttrs (
      # Group credentials
      concatMap (g: map (f: secret "${g}/${f}") groupFields) enabledGroups
      # Per-location peer data
      ++ concatMap (
        e:
        map (f: secret "${e.g}/${e.l}/${f}") (
          [
            "public_key"
            "endpoint"
          ]
          ++ e.extra
        )
      ) entries
    );

    # One .conf per location, rendered at runtime in /run/secrets/rendered (root, 0400)
    sops.templates = listToAttrs (
      map (
        e:
        nameValuePair "${ifName e}.conf" {
          content = ''
            [Interface]
            PrivateKey = ${ph "${e.g}/private_key"}
            Address = ${ph "${e.g}/address"}
            DNS = ${ph "${e.g}/dns"}

            [Peer]
            PublicKey = ${ph "${e.g}/${e.l}/public_key"}
            ${optionalString (elem "preshared_key" e.extra) "PresharedKey = ${ph "${e.g}/${e.l}/preshared_key"}"}
            AllowedIPs = ${ph "${e.g}/allowed_ips"}
            Endpoint = ${ph "${e.g}/${e.l}/endpoint"}
            PersistentKeepalive = 25
          '';
        }
      ) entries
    );

    # One wg-quick interface per location, never started automatically
    networking.wg-quick.interfaces = listToAttrs (
      map (
        e:
        nameValuePair (ifName e) {
          autostart = false;
          configFile = config.sops.templates."${ifName e}.conf".path;
        }
      ) entries
    );

    # Avoids dropped packets caused by wg-quick policy routing
    networking.firewall.checkReversePath = "loose";
  };
}
