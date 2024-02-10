{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.themes;
in {
  imports = [inputs.nix-colors.homeManagerModule];

  config = mkIf (cfg.active == "onedark") {
    gtk = {
      iconTheme = let
        icons =
          if cfg.darkTheme
          then "kora"
          else "kora-light-panel";
      in {
        name = icons;
        package = pkgs.kora-icon-theme;
      };

      cursorTheme = let
        cursor =
          if cfg.darkTheme
          then "Capitaine Cursors"
          else "Capitaine Cursors";
      in {
        name = cursor;
        inherit (cfg.cursor) size;
        package = pkgs.capitaine-cursors-themed;
      };
    };

    colorScheme = let
      inherit (inputs.nix-colors.colorSchemes) onedark one-light;
    in
      if cfg.darkTheme
      then onedark
      else one-light;
  };
}
