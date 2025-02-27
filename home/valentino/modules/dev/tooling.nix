{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with builtins;
let
  cfg = config.valentino.modules.dev.tools;
in
{
  options.valentino.modules.dev.tools.enable = mkEnableOption "Developers tools";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      up-go
      # bruno
      # httpie-desktop
      # mongodb-compass
    ];
  };
}
