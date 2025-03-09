{
  config,
  lib,
  infra,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.outlook;
  inherit (infra.wrappers) pwa;
in
{
  options.valentino.modules.apps.outlook.enable = mkEnableOption "outlook";

  config = mkIf cfg.enable {
    valentino.modules.browsers.chromium.enable = true;

    home.packages = [
      (pwa {
        name = "outlook";
        url = "https://outlook.office.com";
      })
    ];
  };
}
