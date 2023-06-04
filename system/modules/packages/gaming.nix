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
    environment.systemPackages = with pkgs; [
      steam         # A digital distribution platform
      steam-run     # Run commands in the same FHS environment that is used for Steam
      yuzu-mainline # nintendo switch emulator
    ];

    # Enable udev rules for steam controller
    services.udev.packages = with pkgs; [
      sc-controller
    ];
  };
}
