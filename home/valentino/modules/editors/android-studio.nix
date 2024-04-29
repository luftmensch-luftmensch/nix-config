{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.android-studio;
in {
  options.valentino.modules.editors.android-studio.enable = mkEnableOption "Android IDE";

  config = mkIf cfg.enable {
    home.packages = [pkgs.android-studio];
  };
}
