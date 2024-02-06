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
    home.packages = with pkgs; [
      gimp-with-plugins # An image manipulation and paint program
      darktable # Virtual lighttable and darkroom for photographers
    ];
  };
}
