{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.generics;
in {
  options.valentino.modules.dev.generics = {
    enable = mkEnableOption "uncategorized packages for programming";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      httpie-desktop
      insomnium
      mongodb-compass
    ];
  };
}
