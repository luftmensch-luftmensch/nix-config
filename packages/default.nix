{pkgs ? null}: {
  # Httpie Desktop - cross-platform API testing client for humans. Painlessly test REST, GraphQL, and HTTP APIs.
  httpie-desktop = pkgs.callPackage ./httpie-desktop {};

  rofi-powermenu = pkgs.callPackage ./rofi-powermenu {};
  rofi-powermenu-wayland = pkgs.callPackage ./rofi-powermenu {backend = "wayland";};

  san-francisco = pkgs.callPackage ./fonts/san-francisco.nix {};
  icomoon-feather = pkgs.callPackage ./fonts/icomoon-feather.nix {};

  # Custom sddm theme
  sddm-theme-clairvoyance = pkgs.callPackage ./sddm-theme/default.nix {};

  # MPV custom scripts
  mpv-modern-x = pkgs.callPackage ./mpv-scripts/modern-x.nix {};
  mpv-visualizer = pkgs.callPackage ./mpv-scripts/visualizer {};
  mpv-navigator = pkgs.callPackage ./mpv-scripts/file-navigator {};
  mpv-m-x = pkgs.callPackage ./mpv-scripts/M-x {};
}
