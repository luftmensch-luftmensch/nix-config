{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.docker;
in {
  options.system.modules.dev.docker.enable = mkEnableOption "Enable Docker";

  config = mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      storageDriver = "btrfs";
    };
    environment.systemPackages = with pkgs; [docker];
  };
}
