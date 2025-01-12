{
  pkgs,
  lib,
  ...
}: {
  # ref: https://discourse.nixos.org/t/how-to-write-a-electron-app-wrap-function/40581
  # example: discord = wrap { appName = "discord"; }
  wrap = {appName}:
    pkgs.symlinkJoin {
      name = appName;
      paths = [pkgs.${appName}];
      buildInputs = [pkgs.makeWrapper];
      postBuild = lib.strings.concatStrings [
        "wrapProgram $out/bin/"
        appName
        " --add-flags \"--enable-webrtc-pipewire-capturer\""
        " --add-flags \"--enable-features=WaylandWindowDecorations\""
        " --add-flags \"--enable-wayland-ime\""
      ];
    };
}
