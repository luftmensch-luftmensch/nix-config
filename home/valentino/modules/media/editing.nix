{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.media.editing;
  inherit (config.valentino.modules) xorg;
  gimpWrapped = pkgs.symlinkJoin {
    name = "gimp-nogpu";
    paths = [ pkgs.gimp-with-plugins ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/gimp \
        --set GIMP_DISABLE_GPU 1
    '';
  };
in
{
  options.valentino.modules.media.editing.enable = mkEnableOption "Editing tools";
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      darktable
      (if xorg.enable then gimpWrapped else gimp-with-plugins)
      # inkscape-with-extensions
    ];
  };
}
