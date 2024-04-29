{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.media.music;
in {
  options.valentino.modules.media.music.enable = mkEnableOption "an option to play music";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      pavucontrol
      yt-dlp
      spotdl
      spotify
    ];

    # yt-dlp configuration
    home.file.".config/yt-dlp/config".text = ''
      # use .netrc for logins
      # -n

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

      # Leave only title and extension
      -o '%(title)s.%(ext)s'
    '';
  };
}
