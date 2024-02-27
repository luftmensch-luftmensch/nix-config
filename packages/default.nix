{pkgs ? null}: {
  # Httpie Desktop - cross-platform API testing client for humans. Painlessly test REST, GraphQL, and HTTP APIs.
  httpie-desktop = pkgs.callPackage ./httpie-desktop {};

  # Insomnium - Fast local API testing tool that is privacy-focused and 100% local. Painlessly test REST, GraphQL, and HTTP APIs. (Fork of Kong/Insomnia)
  insomnium = pkgs.callPackage ./insomnium {};

  rofi-powermenu = pkgs.callPackage ./rofi-powermenu {};
  theme-toggle = pkgs.callPackage ./theme-toggle {};

  apple-fonts = pkgs.callPackage ./fonts/apple-fonts.nix {};
  icomoon-feather-icons = pkgs.callPackage ./fonts/icomoon-feather-icon.nix {};
  phosphor-icons = pkgs.callPackage ./fonts/phosphor.nix {};
}
