{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.gaming.steam;
in {
  options.valentino.modules.gaming.steam = {
    enable = mkEnableOption "steam support";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      steam
      steam-run
    ];

    # Enable udev rules for steam controller
    # services.udev.packages = with pkgs; [
    #   sc-controller
    # ];
  };
}
