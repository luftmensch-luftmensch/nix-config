{
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
          rev = "38c1e33806fb594a8b8b5f18f1211253f5796526";
          sha256 = "064rps5n75gacdr2z71npv9cnk5n4qc74fbsx0fnczii1pywaxl9";
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
