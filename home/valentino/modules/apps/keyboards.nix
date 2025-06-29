{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.keyboards;
in
{
  options.valentino.modules.apps.keyboards = {
    zmk-studio.enable = mkEnableOption "enable zmk-studio";
  };

  config = mkMerge [
    (mkIf cfg.zmk-studio.enable { home.packages = [ pkgs.zmk-studio ]; })
  ];
}
