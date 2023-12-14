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
      # autotiling               # Manual to dynamic TWM (i3/sway)

      # bitwarden                # A secure and free password manager for all of your devices
      # dunst                    # A customizable and lightweight notification-daemon

      # firefox                  # A web browser built from Firefox source tree
      filezilla                # FTP client
      # fzf                      # A command-line fuzzy finder
      rofi                     # A window  switcher, application launcher, ssh dialog, dmenu replacement and more
      networkmanagerapplet     # NetworkManager control applet
      qrcp                     # Transfer files over wifi by scanning a QR code from your terminal

      tdlib                    # Cross-platform library for building Telegram clients


      # zathura                  # PDF Viewer

    ];
  };
}
