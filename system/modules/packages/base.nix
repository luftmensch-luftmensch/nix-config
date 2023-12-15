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
      filezilla                # FTP client
      rofi                     # A window  switcher, application launcher, ssh dialog, dmenu replacement and more
      networkmanagerapplet     # NetworkManager control applet
      qrcp                     # Transfer files over wifi by scanning a QR code from your terminal

      tdlib                    # Cross-platform library for building Telegram clients
    ];
  };
}
