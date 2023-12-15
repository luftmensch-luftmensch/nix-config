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

    # yt-dlp configuration
    home.file.".config/yt-dlp/config" = {
      enable = true;
      text = ''
        # use .netrc for logins
        #-n

        # ignore errors (unavailable videos, etc)
        -i

        # don't overwrite
        -w

        # make an archive of downloaded videos and only download unlisted items, then list them
        # useful for music playlists you update often
        #--download-archive ~/Music/untagged/ignore.these

        # convert all downloads to 192kbps mp3
        #-x
        #--audio-format mp3 #opus
        #--audio-quality 192K

        # embed thumbnail into audio
        #--embed-thumbnail

        # save all music in the following folder and format
        # -o '~/Music/untagged/%(artist)s - %(title)s.%(ext)s'
      '';
    };
  };
}
