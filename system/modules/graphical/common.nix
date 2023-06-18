{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfgXorg = config.system.modules.graphical.xorg;
  cfgWayland = config.system.modules.graphical.wayland;
in {
  config = mkIf (cfgXorg.enable || cfgWayland.enable) {
    security = {
      pam = {
        services = {
          sddm.enableGnomeKeyring = true;
        };
      };

      polkit.enable = true;
    };

    programs.dconf.enable = true;
    services = {
      dbus = {
        enable = true;
        packages = [pkgs.dconf];
      };

      gnome = {
        gnome-keyring = {
          enable = true;
        };
      };
    };

    # Trash and GTK apps features
    services.gvfs.enable = true;
    services.tumbler.enable = true;
    services.udisks2.enable = true;

    environment.systemPackages = with pkgs; [
      gnome.seahorse
      libsecret
      libinput
    ];

    # To get list of the font installed: (fc-list : family)
    # Valid font names https://github.com/NixOS/nixpkgs/blob/6ba3207643fd27ffa25a172911e3d6825814d155/pkgs/data/fonts/nerdfonts/shas.nix
    fonts.fonts = with pkgs; [
      font-awesome
      fira-code
      monoid # https://larsenwork.com/monoid/
      (nerdfonts.override {fonts = ["Iosevka"];})
      source-code-pro
      sarasa-gothic # A CJK programming font based on Iosevka and Source Han Sans
      victor-mono

      # cantarell_fonts
      # inconsolata
      # mononoki
      # twemoji-color-font
      # scientifica
      # google-fonts
      # noto-fonts-emoji
      # roboto
      # vegur
    ];
  };
}
