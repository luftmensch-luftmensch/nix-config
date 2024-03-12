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
    services.xserver.displayManager.sddm = {
      enable = true;
      theme = "${(pkgs.fetchFromGitHub {
        owner = "luftmensch-luftmensch";
        repo = "sddm-theme";
        rev = "95821cca2b283a83325d46548c41d813dd09d8a1";
        sha256 = "1j0gmvw4ys4brghixrg3x5kp11jdbmdnbgjnfinnr94am8bsb0kv";
      })}";
    };

    environment.systemPackages = with pkgs.libsForQt5.qt5; [qtgraphicaleffects qtsvg];
  };
}
