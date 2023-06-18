{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.sddm;
in {
  options.system.modules.graphical.sddm = {
    enable = mkEnableOption "sddm with dependencies and theme";
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager = {
      sddm = {
        enable = true;
        theme = "${(pkgs.fetchFromGitHub {
          owner = "luftmensch-luftmensch";
          repo = "sddm-theme";
          rev = "d03373b1ddb56f295a587c9d7ffd4647bf739282";
          sha256 = "1n9pd00dmvfq87wcr2nflbpdzviy6nvzrg3kk7d09xsssfh2d1xr";
        })}";
      };
    };

    environment.systemPackages = with pkgs.libsForQt5.qt5; [
      qtgraphicaleffects
      qtsvg
      qtquickcontrols2
    ];
  };
}
