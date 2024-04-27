{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  inherit (config.system.modules.graphical) wayland xorg;
in {
  # XDG Portals, useful for wayland screen sharing and flatpak).
  config = mkMerge [
    (mkIf (xorg.enable || wayland.enable) {
      xdg.portal = {
        enable = true;
        config.common.default = "*";
      };
    })

    (mkIf wayland.enable {
      xdg.portal = {
        extraPortals = [pkgs.xdg-desktop-portal-wlr];
        wlr = {
          enable = true;
          settings.screencast = {
            chooser_type = "simple";
            chooser_cmd = "${pkgs.slurp}/bin/slurp -f %o -or";
          };
        };
      };
    })
  ];
}
