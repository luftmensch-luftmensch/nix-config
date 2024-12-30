{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.themes;
in
{
  imports = [ inputs.nix-colors.homeManagerModule ];

  config = mkIf (cfg.active == "onedark") {
    gtk = {
      iconTheme = {
        name = "Haiku"; # if cfg.darkTheme then "kora" else "kora-light-panel";
        package = pkgs.haiku-icon-theme;
      };

      cursorTheme = {
        name = "Capitaine Cursors";
        inherit (cfg.cursor) size;
        package = pkgs.capitaine-cursors-themed;
      };
    };

    colorScheme =
      let
        inherit (inputs.nix-colors.colorSchemes) onedark one-light;
      in
      if cfg.darkTheme then onedark else one-light;
  };
}
