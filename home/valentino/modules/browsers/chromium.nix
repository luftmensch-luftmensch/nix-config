{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.browsers.chromium;
  inherit (config.valentino.modules) wayland;
  inherit (config.valentino.modules.credentials) bitwarden _1password;
in
{
  options.valentino.modules.browsers.chromium.enable = mkEnableOption "chromium";

  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      # chrome://flags/#enable-webrtc-pipewire-capturer (Enable it to share entire screen)
      commandLineArgs =
        [
          "--force-dark-mode"
          "--no-first-run"
          "--no-default-browser-check"
          "--no-service-autorun"
          "--enable-features=WebUIDarkMode"
        ]
        ++ optionals wayland.enable [
          "--ozone-platform=wayland"
          "--enable-usermedia-screen-capturing"
        ];

      extensions =
        [
          { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock
          { id = "iaiomicjabeggjcfkbimgmglanimpnae"; } # tab manager
        ]
        ++ (lib.optionals bitwarden.enable) [ { id = "nngceckbapebfimnlniiiahkandclblb"; } ]
        ++ (lib.optionals _1password.enable [ { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } ]);
    };
  };
}
