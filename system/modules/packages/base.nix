{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.base;
in {
  options.system.modules.packages.base = {
    enable = mkEnableOption "Enable base packages used across all my devices";
  };

  config = mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      alacritty                # A fast, cross-platform, OpenGL terminal emulator
      autotiling               # Manual to dynamic TWM (i3/sway)

      bitwarden                # A secure and free password manager for all of your devices
      dunst                    # A customizable and lightweight notification-daemon
      imv                      # Image viewer for X11 & Wayland
      inkscape-with-extensions # Vector graphics editor

      firefox                  # A web browser built from Firefox source tree
      filezilla                # FTP client
      fzf                      # A command-line fuzzy finder
      gimp-with-plugins        # An image manipulation and paint program
      darktable                # Virtual lighttable and darkroom for photographers
      (mpv.override {          # Media Player with embedded mpris support
        scripts = [ mpvScripts.mpris ];
      })
      obs-studio               # Free and open source software for video recording and live streaming
      rofi                     # A window  switcher, application launcher, ssh dialog, dmenu replacement and more
      cinnamon.nemo            # Cinnamon file manager
      networkmanagerapplet     # NetworkManager control applet
      pavucontrol              # PulseAudio Volume Control
      qrcp                     # Transfer files over wifi by scanning a QR code from your terminal

      # spotdl                   # Spotify playlists downloader ( Homepage: https://github.com/spotDL/spotify-downloader )
      spotify                  # Play music from the Spotify music service
      tdlib                    # Cross-platform library for building Telegram clients

      yt-dlp                   # A youtube-dl fork with additional features and patches

      zathura                  # PDF Viewer

    ];
  };
}
