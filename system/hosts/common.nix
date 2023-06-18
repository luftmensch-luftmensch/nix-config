{
  pkgs,
  lib,
  ...
}: let
  fs_options = ["autodefrag" "space_cache=v2" "noatime" "compress=zstd:3"];
in {
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 10; # Maximum number of latest generations in the boot menu
        editor = false; # Fix a security hole in place for backwards compatibility (permit sudo privileges when booting)
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
      timeout = 2;
    };

    initrd = {
      availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "ums_realtek" "usbhid" "usb_storage" "sd_mod" "xxhash" "nvme" "sd_mod" "rtsx_pci_sdmmc"];

      # Note `lib.mkBefore` is used instead of `lib.mkAfter` here.
      postDeviceCommands = pkgs.lib.mkBefore ''
        mkdir -p /mnt
        mount -o subvol=/ /dev/mapper/nix-enc /mnt

        btrfs subvolume list -o /mnt/@ |
        cut -f9 -d' ' |
        while read subvolume; do
          echo "DELETING /$subvolume SUBVOLUME..."
          btrfs subvolume delete "/mnt/$subvolume"
        done &&
        echo "DELETING @ SUBVOLUME..." &&
        btrfs subvolume delete /mnt/@

        echo "RESTORING BLANK /root SUBVOLUME..."
        btrfs subvolume snapshot /mnt/root-blank /mnt/@

        umount /mnt
      '';
    };
  };

  # An handy way to retrieve the corresponding label from UUID is using `blkid`
  fileSystems."/" = {
    device = "/dev/disk/by-label/vault";
    fsType = "btrfs";
    # options = [ "subvol=@" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:3" ];
    options = ["subvol=@"] ++ fs_options;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/vault";
    fsType = "btrfs";
    options = ["subvol=@home"] ++ fs_options;
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/vault";
    fsType = "btrfs";
    options = ["subvol=@nix"] ++ fs_options;
  };

  fileSystems."/persist" = {
    device = "/dev/disk/by-label/vault";
    fsType = "btrfs";
    options = ["subvol=@persist"] ++ fs_options;
    neededForBoot = true;
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-label/vault";
    fsType = "btrfs";
    options = ["subvol=@var_log"] ++ fs_options;
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{device = "/dev/disk/by-label/SWAP";}];

  services.btrfs.autoScrub = {
    enable = true;
    interval = "monthly";
  };
}
