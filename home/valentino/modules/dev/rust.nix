{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.dev.rust;
in
{
  options.valentino.modules.dev.rust.enable =
    mkEnableOption "rust language support and language server";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.rustup ];
  };
}
