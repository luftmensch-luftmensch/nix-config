{
  config,
  lib,
  infra,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.teams;
  inherit (infra.wrappers) pwa;
in
{
  options.valentino.modules.apps.teams.enable = mkEnableOption "teams";

  config = mkIf cfg.enable {
    valentino.modules.browsers.chromium.enable = true;

    home.packages = [
      (pwa {
        name = "teams";
        url = "https://teams.microsoft.com";
      })
    ];
  };
}
