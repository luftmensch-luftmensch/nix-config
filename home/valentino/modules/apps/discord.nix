{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.discord;

  discord-chromium = pkgs.makeDesktopItem {
    name = "Discord";
    desktopName = "Discord";
    genericName = "All-in-one cross-platform voice and text chat for gamers";
    exec = ''
      ${config.programs.chromium.package}/bin/chromium --ozone-platform-hint=auto --app="https://discord.com/channels/@me"'';
    icon = "discord";
    type = "Application";
    categories = ["Network" "InstantMessaging"];
    terminal = false;
    mimeTypes = ["x-scheme-handler/discord"];
  };
in {
  options.valentino.modules.apps.discord = {
    enable = mkEnableOption "discord";
  };

  config = mkIf cfg.enable {
    valentino.modules.browsers.chromium.enable = true;

    home.packages = [
      # To fix copy/paste enable the Legacy chat input from Settings
      discord-chromium
    ];
  };
}
