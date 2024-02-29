{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.browsers.chromium;
  inherit (config.valentino.modules) wayland xorg;
in {
  options.valentino.modules.browsers.chromium = {
    enable = mkEnableOption "chromium";
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
          "--enable-features=UseOzonePlatform"
          "--ozone-platform=wayland"
          "--enable-features=WebRTCPipeWireCapturer"
          "--enable-usermedia-screen-capturing"
        ]
        ++ optionals xorg.enable ["--ozone-platform-hint=auto"];

      extensions = [
        {id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";} # ublock
        {id = "nngceckbapebfimnlniiiahkandclblb";} # bitwarden
        {id = "iaiomicjabeggjcfkbimgmglanimpnae";} # tab manager
      ];
    };
  };
}
