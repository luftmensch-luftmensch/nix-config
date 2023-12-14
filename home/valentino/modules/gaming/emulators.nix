{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.gaming.emulators;
in {
  options.valentino.modules.gaming.emulators = {
    switch.enable = mkEnableOption "switch emulator";
  };

  config = {
    home.packages = with pkgs; [(mkIf cfg.switch.enable yuzu-mainline)];
  };
}
