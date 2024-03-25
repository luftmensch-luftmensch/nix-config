{pkgs ? null}: {
  # Httpie Desktop - cross-platform API testing client for humans. Painlessly test REST, GraphQL, and HTTP APIs.
  httpie-desktop = pkgs.callPackage ./httpie-desktop {};

  # Insomnium - Fast local API testing tool that is privacy-focused and 100% local. Painlessly test REST, GraphQL, and HTTP APIs. (Fork of Kong/Insomnia)
  insomnium = pkgs.callPackage ./insomnium {};

  rofi-powermenu = pkgs.callPackage ./rofi-powermenu {};
  # theme-toggle = pkgs.callPackage ./theme-toggle {};

  san-francisco = pkgs.callPackage ./fonts/san-francisco.nix {};
  icomoon-feather-icons = pkgs.callPackage ./fonts/icomoon-feather-icon.nix {};
  phosphor-icons = pkgs.callPackage ./fonts/phosphor.nix {};

  # Custom sddm theme
  sddm-theme-clairvoyance = pkgs.callPackage ./sddm-theme/default.nix {};

  # MPV custom scripts
  mpv-modern-x = pkgs.callPackage ./mpv-scripts/modern-x.nix {};
  mpv-visualizer = pkgs.callPackage ./mpv-scripts/visualizer {};
  mpv-navigator = pkgs.callPackage ./mpv-scripts/file-navigator {};
  mpv-m-x = pkgs.callPackage ./mpv-scripts/M-x {};
}
