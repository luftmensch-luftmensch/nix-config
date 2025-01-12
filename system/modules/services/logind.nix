{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.services.logind;
in {
  options.system.modules.services.logind.enable = mkEnableOption "Enable logind capabilities";

  config = mkIf cfg.enable {
    services.logind = {
      lidSwitch = "suspend";
      lidSwitchDocked = "suspend";
    };
  };
}
