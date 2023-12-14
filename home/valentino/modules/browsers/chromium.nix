{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.browsers.chromium;
  cfgWayland = config.valentino.modules.wayland;
in {
  options.valentino.modules.browsers.chromium = {
    enable = mkEnableOption "chromium";
  };

  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      commandLineArgs =
        if cfgWayland.enable then [
          "--enable-features=UseOzonePlatform"
          "--ozone-platform=wayland"
          "--enable-features=WebRTCPipeWireCapturer"
          "--enable-usermedia-screen-capturing"
        ]
        else ["--ozone-platform-hint=auto"]
      ;
    };
  };
}
