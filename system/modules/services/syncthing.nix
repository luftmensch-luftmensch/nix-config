{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.services.syncthing;
  cfgUser = config.system.modules.core.user.username;
in {
  options.system.modules.services.syncthing = {
    enable = mkEnableOption "Enable syncthing capabilities";
    device-id = mkOption {
      type = types.str;
      default = "";
      description = lib.mdDoc ''
        The device ID. See <https://docs.syncthing.net/dev/device-ids.html>.
      '';
    };
  };

  config = mkIf cfg.enable {
    services = {
      syncthing = {
        enable = true;
        user = cfgUser;
        configDir = "/home/valentino/.config/syncthing";
        dataDir = "/home/valentino/.config/syncthing";
        openDefaultPorts = true; # TCP 22000 for transfer, UDP 21027 for discovery

        overrideFolders = true; # Purge folders not declaratively configured!
        overrideDevices = true;
        #relay.enable = true;

				settings = {
					devices = {
						P30-PRO = {
							id = "POGJUQZ-LA6JNGT-T7VN6AL-ZYVOEGE-HHNDWPN-6SXXULO-IQKO7KQ-6HNPBQP"; # P30-PRO
						};
						nixos-device = {
							id = cfg.device-id;
						};
					};
					folders = {
						"Music" = {
							path = "/home/valentino/Music";
							id = "n4gcw-3q7u5";
							devices = ["nixos-device"];
						};

						"Dropbox" = {
							path = "/home/valentino/Dropbox";
							id = "tcfun-ya2ir";
							devices = ["P30-PRO" "nixos-device"]; # NAS
						};

						"Video" = {
							path = "/home/valentino/Video";
							id = "tzf49-nwpwz";
							devices = ["nixos-device"]; # PC
						};
					};
				};

        # extraOptions = {
        #   gui = {
        #     theme = "black";
        #   };
        # };

      };
    };
  };
}
