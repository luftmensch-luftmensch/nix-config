{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.secrets;
in {
  options.system.modules.packages.secrets = {
    enable = mkEnableOption "Enable packages for secrets";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      age  # Modern encryption tool with small explicit keys
      sops # editor of encrypted files
    ];
  };
}
