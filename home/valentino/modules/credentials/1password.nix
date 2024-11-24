{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.credentials._1password;
in
{
  options.valentino.modules.credentials._1password.enable = mkEnableOption "1password";

  config = mkIf cfg.enable {
    programs._1password.enable = true;

    home.packages = [ pkgs._1password ];
  };
}
