{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.apps.discord;
  inherit (infra.wrappers) pwa;
in
{
  options.valentino.modules.apps.discord.enable = mkEnableOption "discord";

  config = mkIf cfg.enable {
    valentino.modules.browsers.chromium.enable = true;

    home.packages = [
      (pwa {
        name = "outlook";
        url = "https://discord.com/channels/@me";
      })
    ];
  };
}
