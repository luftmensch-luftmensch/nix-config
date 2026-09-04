{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.wayland.wob;
in
{
  options.valentino.modules.wayland.wob.enable =
    mkEnableOption "Overlay volume/backlight/progress/anything bar for Wayland";

  config = mkIf cfg.enable {
    stylix.targets.wob.enable = true;
    services.wob.enable = true;
  };
}
