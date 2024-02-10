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
          rev = "32ae9e1aec7270290eeb6df6a1aaf7542b49793d";
          sha256 = "1w187bgv6fflax39ag3ifglsdy33127vz3ssd8axslvsj375668y";
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
