{
  config,
  lib,
  pkgs,
  unstable-pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.vnc;
in
{
  options.valentino.modules.apps.vnc.enable = mkEnableOption "enable vnc capabilities";

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.remmina
      unstable-pkgs.anydesk
    ];
  };
}
