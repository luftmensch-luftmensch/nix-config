{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.gaming;
in {
  options.system.modules.packages.gaming = {
    enable = mkEnableOption "Enable gaming capabilities";
  };

  config = mkIf cfg.enable {
  };
}
