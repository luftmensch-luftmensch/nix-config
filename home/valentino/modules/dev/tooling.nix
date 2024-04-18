{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with builtins; let
  cfg = config.valentino.modules.dev.tools;
in {
  options.valentino.modules.dev.tools = {
    enable = mkEnableOption "Developers tools";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      bruno
      httpie-desktop
      insomnium
      mongodb-compass
      # TODO: Enable it when buildGoModule 1.22 lands on stable
      # rHttp
    ];
  };
}
