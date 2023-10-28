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
      at            # The classical Unix `at' job scheduling command   
      bat           # After any updates of themes/syntaxes run `bat cache --build`
      bind          # Domain name server
      bluez         # Bluetooth support for Linux
      exa           # A better alternative to boring ls
      fd            # Alternative to find (much faster)
      ffmpeg-full   # ffmpeg video converter
      fs-diff       # Bash script to show a diff between the root subvolume and the root-blank subvolume


      git           # gitAndTools.gitFull
      git-lfs       # gitAndTools.gitFull
      htop          # Why is my laptop so hot? Oh yeah I'm compiling 20 programs while running two games
      jq            # command-line JSON processor
      hugo          # Static website engine
      imagemagick   # File conversion & manipulation

      lm_sensors    # Tools for reading hardware sensors
      lshw          # Info on the HW configuration of the machine
      lsof          # List open files
      nmap          # Network exploration tool and security / port scanner

      procs         # Modern replacement for ps
      ps_mem        # Usage: sudo ps_mem -p $(pgrep -d, -u $USER) (Why is Emacs using so much RAM?)
      ripgrep       # Blazing fast grep
      speedtest-cli # Command line interface for testing internet bandwidth using speedtest.net

      tmpmail       # Temporary email (https://github.com/sdushantha/tmpmail)
      traceroute    # Print the route packets trace to network host

      unrar         # Utility for RAR archives
      unzip         # list, test and extract compressed files in a ZIP archive
      usbutils      # Tools for working with USB devices, such as lsusb

      wget          # The non-interactive network downloader
      wavemon       # Ncurses-based monitoring application for wireless network devices

      zip           # package and compress (archive) files

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
