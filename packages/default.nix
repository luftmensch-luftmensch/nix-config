{
  pkgs ? null,
}:
{
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu { };
  san-francisco = pkgs.callPackage ./fonts/san-francisco.nix { };
  sf-mono = pkgs.callPackage ./fonts/sf-mono.nix { };

  firefox-parfait = pkgs.callPackage ./firefox/firefox-parfait.nix { };
  haiku-icon-theme = pkgs.callPackage ./icons/haiku.nix { };

  # Custom sddm theme
  sddm-theme-silent = pkgs.callPackage ./sddm { theme = "rei"; };

  # MPV custom scripts
  mpv-navigator = pkgs.callPackage ./mpv-scripts/file-navigator.nix { };
  mpv-m-x = pkgs.callPackage ./mpv-scripts/M-x.nix { };
  mdx-go = pkgs.callPackage ./mdx.nix { };
  up-go = pkgs.callPackage ./up.nix { };
}
