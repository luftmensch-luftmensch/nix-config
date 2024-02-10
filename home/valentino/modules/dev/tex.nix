{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.tex;
in {
  options.valentino.modules.dev.tex = {
    enable = mkEnableOption "Tex support";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      texlive.combined.scheme-full
      tectonic
    ];
  };
}
