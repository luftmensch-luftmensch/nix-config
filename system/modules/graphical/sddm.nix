{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.sddm;
in {
  options.system.modules.graphical.sddm.enable = mkEnableOption "sddm with dependencies and theme";

  config = mkIf cfg.enable {
    services.xserver.displayManager.sddm = {
      enable = true;
      theme = "clairvoyance";
    };

    environment.systemPackages = with pkgs;
      [sddm-theme-clairvoyance] # Personal custom theme
      ++ (with libsForQt5.qt5; [qtgraphicaleffects qtsvg]);
  };
}
