{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.ricing;
in {
  options.system.modules.packages.ricing = {
    enable = mkEnableOption "Enable packages for ricing";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      capitaine-cursors         # An x-cursor theme inspired by macOS and based on KDE Breeze

      dconf                     # Low-level configuration system

      gnome3.adwaita-icon-theme # Make sure that the default icon theme is avaliable to gtk applications
      gtk3                      # In substitution of gnome.gtk (removed on 13-01-2022)
      gnome3.gnome-disk-utility # A udisks graphical front-end
      gnome-themes-extra        # Needed to fix `Unable to locate theme engine in module_path: "adwaita"` (https://github.com/NixOS/nixpkgs/issues/60918)

      materia-theme             # Material Design theme for GNOME/GTK based desktop environments
      papirus-icon-theme        # Open source SVG icon theme for Linux

      # Qt5 Configuration Tool
      qt5ct libsForQt5.qtstyleplugin-kvantum libsForQt5.breeze-qt5

    ];
  };
}
