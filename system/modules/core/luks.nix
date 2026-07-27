{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.system.modules.core.boot.luks;
in
{
  # Sometimes it is necessary to boot a system without needing an keyboard and monitor.
  # You will create a secret key, add it to a key slot and put it onto an USB stick.
  # (even better using the by-label format identifier)
  # Then save the key into the usb stick chosen
  # Then edit the initrd options and rebuild the system.
  # Below the commands used to do it

  # dd if=/dev/urandom of=usb.key bs=4096 count=1
  # cryptsetup luksAddKey LUKSDISK ./usb.kek
  # dd if=usb.key of=/dev/disk/by-id/USB_STICK

  options.system.modules.core.boot.luks = {
    enable = mkEnableOption "luks config";
    keyFile = mkOption {
      type = types.str;
      description = "Which keyfile to use";
    };

    keyFileTimeout = mkOption {
      type = types.int;
      default = 10;
      description = "Seconds to wait for the keyFile device before failing back to password prompt";
    };
  };

  config = mkIf cfg.enable {
    boot.initrd.luks.devices."nix-enc" = {
      device = "/dev/disk/by-label/nix-enc";
      preLVM = true;
      allowDiscards = true;
      keyFileSize = 4096;
      keyFile = "${cfg.keyFile}";
      inherit (cfg) keyFileTimeout;
    };
  };
}
