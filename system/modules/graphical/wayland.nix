{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.wayland;
in {
  options.system.modules.graphical.wayland = {
    enable = mkEnableOption "Wayland basic configuration and packages";
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager.defaultSession = "sway";
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      extraPackages = lib.mkDefault [];
    };
  };
}
