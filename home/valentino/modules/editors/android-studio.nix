{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.android;
in {
  options.valentino.modules.editors.android = {
    enable = mkEnableOption "Android IDE and screen mirroring";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      android-studio
    ];
  };
}
