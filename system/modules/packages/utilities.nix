{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.utilities;

  fs-diff = pkgs.writeShellScriptBin "fs-diff" ''
    #!/usr/bin/env bash
    # fs-diff.sh
    set -euo pipefail
    sudo mkdir -p /mnt

    sudo mount -o subvol=/ /dev/mapper/nix-enc /mnt
    OLD_TRANSID=$(sudo btrfs subvolume find-new /mnt/root-blank 9999999)
    OLD_TRANSID=$(echo $OLD_TRANSID | awk -F' ' '{print $4}')

    sudo btrfs subvolume find-new "/mnt/@" "$OLD_TRANSID" |
    sed '$d' |
    cut -f17- -d' ' |
    sort |
    uniq |
    while read path; do
      path="/$path"
        if [ -L "$path" ]; then
      : # The path is a symbolic link, so is probably handled by NixOS already
        elif [ -d "$path" ]; then
            : # The path is a directory, ignore
        else
          echo "$path"
        fi
       done

    sudo umount -R /mnt
  '';


in {
  options.system.modules.packages.utilities = {
    enable = mkEnableOption "Enable utilities packages ";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # at            # The classical Unix `at' job scheduling command   
      bluez         # Bluetooth support for Linux
      # fs-diff       # Bash script to show a diff between the root subvolume and the root-blank subvolume


      # git           # gitAndTools.gitFull
      # git-lfs       # gitAndTools.gitFull
      # hugo          # Static website engine
      # bc            # GNU software calculator
      # usbguard      # Protect against infected USB
      # gh            # Github's CLI tool

      # killall

      # mkpasswd      # Generates passwords and can apply them automatically to users
      # mpg123
      # telnet        # client-server protocol predating the TCP protocol.
      # foremost      # Data recovery tool
      # appimage-run  # Execute appimage programs
    ];
  };
}
