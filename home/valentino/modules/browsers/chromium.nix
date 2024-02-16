{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.browsers.chromium;
  inherit (config.valentino.modules) wayland;
in {
  options.valentino.modules.browsers.chromium = {
    enable = mkEnableOption "chromium";
  };

  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      commandLineArgs =
        # chrome://flags/#enable-webrtc-pipewire-capturer (Enable it to share entire screen)
        if wayland.enable
        then [
          "--enable-features=UseOzonePlatform"
          "--ozone-platform=wayland"
          "--enable-features=WebRTCPipeWireCapturer"
          "--enable-usermedia-screen-capturing"
        ]
        else ["--ozone-platform-hint=auto"];

      extensions = [
        {id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";} # ublock
        {id = "nngceckbapebfimnlniiiahkandclblb";} # bitwarden
        {id = "iaiomicjabeggjcfkbimgmglanimpnae";} # tab manager
      ];
    };
  };
}
