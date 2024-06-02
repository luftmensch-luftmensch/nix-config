{ config, lib, ... }:
with lib;
let
  cfg = config.system.modules.services.touchpad;
in
{
  options.system.modules.services.touchpad.enable = mkEnableOption "Enable touchpad capabilities";

  config = mkIf cfg.enable {
    services.libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };
  };
}
