{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.tex;
in {
  options.valentino.modules.dev.tex.enable = mkEnableOption "Tex support";

  config = mkIf cfg.enable {
    # NOTE: To find out where a specifc sty is located: ll $(dirname $(kpsewhich listings.sty))
    home.packages = with pkgs; [texlive.combined.scheme-full tectonic];
  };
}
