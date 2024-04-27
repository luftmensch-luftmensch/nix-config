{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.services.syncthing;
  user = config.system.modules.core.user.username;
  availableThemes = ["default" "black" "dark"];
in {
  options.system.modules.services.syncthing = {
    enable = mkEnableOption "Enable syncthing capabilities";
    id = mkOption {
      type = types.str;
      default = "";
      description = lib.mdDoc ''
        The device ID. See <https://docs.syncthing.net/dev/device-ids.html>.
      '';
    };
    theme = mkOption {
      type = lib.types.enum availableThemes;
      default = "black";
      description = ''
        Name of the theme to use for the Web UI.

        Supported themes are: ${builtins.toString availableThemes}
      '';
    };
  };

  config = mkIf cfg.enable {
    services = {
      syncthing = let
        dir = "/home/${user}/.config/syncthing";
      in {
        enable = true;
        inherit user;
        configDir = dir;
        dataDir = dir;
        openDefaultPorts = true; # TCP 22000 for transfer, UDP 21027 for discovery
        overrideFolders = true; # Purge folders not declaratively configured!
        overrideDevices = true;
        # relay.enable = true;

        settings = {
          gui.theme = cfg.theme;
          devices = {
            P30-PRO.id = "POGJUQZ-LA6JNGT-T7VN6AL-ZYVOEGE-HHNDWPN-6SXXULO-IQKO7KQ-6HNPBQP";
            nixos-device = {
              inherit (cfg) id;
            };
          };
          folders = {
            "Dropbox" = {
              path = "/home/${user}/Dropbox";
              id = "tcfun-ya2ir";
              devices = ["P30-PRO" "nixos-device"];
            };

            "Video" = {
              path = "/home/${user}/Video";
              id = "tzf49-nwpwz";
              devices = ["nixos-device"];
            };
          };
        };
      };
    };
  };
}
