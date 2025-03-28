{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.credentials.bitwarden;
in
{
  options.valentino.modules.credentials.bitwarden.enable = mkEnableOption "bitwarden";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.bitwarden ];
  };
}
