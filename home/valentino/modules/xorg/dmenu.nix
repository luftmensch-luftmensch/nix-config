{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg.dmenu;
in {
  options.valentino.modules.xorg.dmenu = {
    enable = mkEnableOption "Dmenu w/ additional packages";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      (dmenu.overrideAttrs (oldAttrs: {
        patches = [
          ./patches/case-insensitive
          ./patches/borders
        ];
      }))
    ];
  };
}
