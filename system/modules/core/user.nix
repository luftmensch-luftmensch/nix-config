{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.core.user;
  inherit (config.system.modules.dev) adb docker virtualisation;
  inherit (config.system.modules.services.printing) cups sane;
  inherit (config.system.modules.services) touchpad;
  inherit (config.networking) networkmanager;
in {
  options.system.modules.core.user = {
    enable = mkEnableOption "Enable user configuration";
    uid = mkOption {
      type = types.nullOr types.int;
      default = 1000;
      description = "User custom id";
    };

    username = mkOption {
      type = types.str;
      default = "valentino";
      description = "User username";
    };

    description = mkOption {
      type = types.str;
      default = "";
      description = "Realname of the user";
    };

    extraGroups = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional groups for the user. More info at https://wiki.debian.org/SystemGroups";
    };

    extraAuthorizedKeys = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional authorized keys.";
    };

    extraRootAuthorizedKeys = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional authorized keys for root user.";
    };

    hashedPassword = mkOption {
      type = types.str;
      default = "!";
      description = "Enable hashed password for the user";
    };
  };

  config = mkIf cfg.enable {
    users.users = {
      # disable root login here (nothing hashes to !), and also when installing nix by running nixos-install --no-root-passwd
      # https://discourse.nixos.org/t/how-to-disable-root-user-account-in-configuration-nix/13235/3
      root.hashedPassword = "!";

      ${cfg.username} = {
        inherit (cfg) description hashedPassword uid;
        isNormalUser = true;
        extraGroups =
          ["wheel" "plugdev"]
          ++ optionals adb.enable ["adbusers"]
          ++ optionals docker.enable ["docker"]
          ++ optionals networkmanager.enable ["networkmanager"]
          ++ optionals virtualisation.enable ["libvirtd"]
          ++ optionals cups.enable ["lp" "lpadmin"]
          ++ optionals sane.enable ["scanner"]
          ++ optionals touchpad.enable ["input"]
          ++ cfg.extraGroups;

        openssh.authorizedKeys.keys =
          [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIXljN45Z1tPnPH0ow3i/w2hCKcc8Q2/KPTB+yl30X7R valentino@pine64"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII5bHKpkOWDHNEaG5eovp8pQzsNpJIm8+ziHwF5idLKf valentino@P30-Pro"
          ]
          ++ cfg.extraAuthorizedKeys;
      };
    };

    # Session variables
    environment.sessionVariables = {
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
    };
  };
}
