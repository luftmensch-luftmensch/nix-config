{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.system.modules.graphical.sddm;
  inherit (config.system.modules.core.user) username;
  home = config.users.users.${username}.home;
in
{
  options.system.modules.graphical.sddm.enable = mkEnableOption "sddm with dependencies and theme";

  config = mkIf cfg.enable {
    services.displayManager.sddm = {
      enable = true;
      theme = "silent";
      # For some reason this is needed (Thanks to https://github.com/NixOS/nixpkgs/issues/390251)
      extraPackages = [ pkgs.sddm-theme-silent ];
    };

    environment.systemPackages =
      with pkgs;
      [ sddm-theme-silent ] # Personal custom theme
      ++ (with pkgs.kdePackages; [
        qtmultimedia
        qtsvg
        qtvirtualkeyboard
      ]);

    # Custom avatar for sddm (Taken from https://github.com/thomX75/nixos-modules/blob/main/SDDM/sddm-avatar.nix)
    systemd = {
      services."sddm-avatar" = {
        description = "Service to copy or update users Avatars at startup.";
        wantedBy = [ "multi-user.target" ];
        before = [ "sddm.service" ];
        script =
          let
            source = "${home}/nix-config/assets/avatar.png";
            dest = "/var/lib/AccountsService/icons/${username}";
          in
          ''
            if [ -f "${source}" ]; then
              if [ ! -f "${dest}" ] || ! cmp -s "${source}" "${dest}"; then
                  rm -f "${dest}"
                  cp -L "${source}" "${dest}"
              fi
            fi
          '';
        serviceConfig = {
          Type = "simple";
          User = "root";
          StandardOutput = "journal+console";
          StandardError = "journal+console";
        };
      };

      # Ensures SDDM starts after the service.
      services.sddm.after = [ "sddm-avatar.service" ];
    };
  };
}
