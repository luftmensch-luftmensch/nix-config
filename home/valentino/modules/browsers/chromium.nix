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
  options.valentino.modules.browsers.chromium = {
    enable = mkEnableOption "chromium";
    ungoogled = mkEnableOption "UnGoogled features";
  };

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

          # TODO: try out chrome wayland flags
          # "--ignore-gpu-blocklist"
          # "--enable-gpu-rasterization"
          # "--enable-zero-copy"
          # "--disable-features=UseChromeOSDirectVideoDecoder"
          # "--enable-media-router"
          # "--enable-smooth-scrolling"
        ]
        ++ optionals cfg.ungoogled [
          # TODO: Try out ungoogled chrome flag features
          "--disable-search-engine-collection"
          "--extension-mime-request-handling=always-prompt-for-install"
          "--fingerprinting-canvas-image-data-noise"
          "--fingerprinting-canvas-measuretext-noise"
          "--fingerprinting-client-rects-noise"
          "--popups-to-tabs"
          "--force-punycode-hostnames"
          "--show-avatar-button=incognito-and-guest"
          "--no-pings"
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
