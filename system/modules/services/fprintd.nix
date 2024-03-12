{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.services.fingerprint;
in {
  options.system.modules.services.fingerprint = {
    enable = mkEnableOption "Enable fprintd capabilities";
  };

  config = mkIf cfg.enable {
    services.fprintd.enable = true;
    security.pam.services.swaylock.fprintAuth = true;
  };
}
