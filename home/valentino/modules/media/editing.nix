{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.editing;
in {
  options.valentino.modules.media.editing = {
    enable = mkEnableOption "an option for editing";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [darktable gimp-with-plugins inkscape-with-extensions];
  };
}
