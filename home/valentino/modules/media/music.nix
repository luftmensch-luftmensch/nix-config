{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.music;
in {
  options.valentino.modules.media.music = {
    enable = mkEnableOption "an option to play music";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      pavucontrol # PulseAudio Volume Control
      yt-dlp # A youtube-dl fork with additional features and patches
      spotdl # Spotify playlists downloader
      spotify # Play music from the Spotify music service
    ];
  };
}
