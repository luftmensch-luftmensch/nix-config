{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.gaming.emulators;
in {
  options.valentino.modules.gaming.emulators.switch.enable = mkEnableOption "switch emulator";

  config = mkIf cfg.switch.enable {
    home.packages = [pkgs.yuzu-mainline];
  };
}
