{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.java;
in {
  options.valentino.modules.dev.java.enable = mkEnableOption "java support with battery included";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [jdk maven gradle];
  };
}
