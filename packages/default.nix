{
  pkgs ? null,
}:
{
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu { };
  rofi-powermenu-wayland = pkgs.callPackage ./rofi-powermenu { backend = "wayland"; };

  san-francisco = pkgs.callPackage ./fonts/san-francisco.nix { };

  # Custom sddm theme
  sddm-theme-clairvoyance = pkgs.callPackage ./sddm-theme/default.nix { };

  # MPV custom scripts
  mpv-navigator = pkgs.callPackage ./mpv-scripts/file-navigator.nix { };
  mpv-m-x = pkgs.callPackage ./mpv-scripts/M-x.nix { };
  meteor-go = pkgs.callPackage ./meteor.nix { };
  mdx-go = pkgs.callPackage ./mdx.nix { };
  up-go = pkgs.callPackage ./up.nix { };
}
