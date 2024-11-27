{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.remmina;
in
{
  options.valentino.modules.apps.remmina.enable = mkEnableOption "playerct";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.remmina ];
  };
}
