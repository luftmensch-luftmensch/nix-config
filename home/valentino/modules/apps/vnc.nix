{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.vnc;
in
{
  options.valentino.modules.apps.vnc.enable = mkEnableOption "enable vnc capabilities";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      remmina
      anydesk
    ];
  };
}
