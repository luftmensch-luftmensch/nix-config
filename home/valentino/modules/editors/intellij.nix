{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.intellij;
in {
  options.valentino.modules.editors.intellij = {
    enable = mkEnableOption "Intellij";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.jetbrains; [
      idea-community
    ];
  };
}
