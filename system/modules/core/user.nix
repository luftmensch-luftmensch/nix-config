{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.core.user;
in {
  options.system.modules.core.user = {
    enable = mkEnableOption "Enable user configuration";
    uid = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = 1000;
      description = "User custom id";
    };

    username = lib.mkOption {
      type = lib.types.str;
      default = "valentino";
      description = "User username";
    };

    description = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Realname of the user";
    };

    extraGroups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
    };

    extraAuthorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Additional authorized keys.";
    };

    extraRootAuthorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Additional authorized keys for root user.";
    };

    hashedPassword = lib.mkOption {
      type = lib.types.str;
      default = "!";
      description = "Enable hashed password for the user";
    };
  };

  config = mkIf cfg.enable {
    users.users = {
      root = {
        # disable root login here, and also when installing nix by running nixos-install --no-root-passwd
        # https://discourse.nixos.org/t/how-to-disable-root-user-account-in-configuration-nix/13235/3
        hashedPassword = "!"; # disable root logins, nothing hashes to !
      };

      ${cfg.username} = {
        inherit (cfg) description hashedPassword uid;
        isNormalUser = true;
        extraGroups = ["wheel"] ++ cfg.extraGroups;

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
      XDG_BIN_HOME = "$HOME/.local/bin";
      XDG_STATE_HOME = "$HOME/.local/state";
    };
  };
}
