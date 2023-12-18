{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.miscellaneous;
in {
  options.system.modules.packages.miscellaneous = {
    enable = mkEnableOption "Enable uncategorized packages";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # openssl                              # Cryptography toolkit implementing TLS network protocol
    ];
  };
}
