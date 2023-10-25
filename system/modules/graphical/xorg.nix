{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.xorg;
in {
  options.system.modules.graphical.xorg = {
    enable = mkEnableOption "xorg basic configuration and packages";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      layout = "it";
      libinput.enable = true;
      displayManager.defaultSession = "none+i3";
      windowManager = {
        i3.enable = true;
      };
    };
  };
}
