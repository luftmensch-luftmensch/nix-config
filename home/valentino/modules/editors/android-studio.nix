{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.android;
in {
  options.valentino.modules.editors.android = {
    enable = mkEnableOption "Android IDE";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      android-studio
    ];
  };
}
