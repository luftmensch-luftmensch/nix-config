{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  # cfgXorg = config.system.modules.graphical.xorg;
  # cfgWayland = config.system.modules.graphical.wayland;
  inherit (config.system.modules.graphical) xorg wayland;
in {
  config = mkIf (xorg.enable || wayland.enable) {
    security = {
      pam.services.sddm.enableGnomeKeyring = true;
      polkit.enable = true;
    };

    programs.dconf.enable = true;
    services = {
      dbus = {
        enable = true;
        packages = [pkgs.dconf];
      };

      gnome.gnome-keyring.enable = true;
    };

    # Trash and GTK apps features
    services = {
      gvfs.enable = true;
      tumbler.enable = true;
      udisks2.enable = true;
    };

    # Who the hell uses xterm these days?
    services.xserver.excludePackages = [pkgs.xterm];

    environment.systemPackages = with pkgs; [gnome.seahorse libsecret libinput];
  };
}
