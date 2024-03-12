#!/usr/bin/env bash
set -e # exit immediately if a command return non-zero status

# Global variables
declare -a subvolumes_list=("@" "@home" "@nix" "@persist" "@var_log")
declare -a subvolumes_flags=("compress=zstd:3" "noatime" "autodefrag" "space_cache=v2" "discard=async")

log() {
    tostdout () {
        printf "%*s%b%s %s %s\033[0m%b" "$INDENT" '' "${COLOR}" "${HEADER}" "${SEPARATOR}" "${MESSAGE}" "${NEWLINE:-\n}";
    }

    # No message given
    if [[ $# -eq 0 ]]; then
        log E "No parameters were given to the logmsg function!";
        return 0
    fi

    FORMAT="${1}"; shift;
    MESSAGE="${1}"; shift;
    INDENT=${INDENT:-0}

    shopt -s nocasematch
    case "$FORMAT" in
        'E')  HEADER="[ERR] "; COLOR="\033[0;31m"; SEPARATOR="»"; tostdout ;;
        'W')  HEADER="[WARN]"; COLOR="\033[0;33m"; SEPARATOR="»"; tostdout ;;
        'I')  HEADER="[INFO]"; COLOR="\033[0;32m"; SEPARATOR="»"; tostdout ;;
        *) log E "Internal $(basename "$PROGNAME") error: format ‹${FORMAT}› does not exist!";;
    esac
    shopt -u nocasematch
}

info () {
    printf "This script generate a full working Nixos installation with encrypted BTRFS partition as root\n"
    printf "This script will wipe the given device, these are the partitions that will be created\n"

    printf "1: boot partition (vfat ~ 1024M ~ /boot)\n"
    printf "3: encrypted luks root partition (btrfs ~ rest of the disk ~ /dev/mapper/nix-enc):\n"

    printf "\t%-10s\t%-10s\t%-30s\n" "Subvolumes" "Mount Point" "Flags"
    for i in "${subvolumes_list[@]}"; do
        printf "\t%-10s\t%-10s\t%-30s\n" "$i" "${i#?}" "${subvolumes_flags[*]}"
    done
    printf "\e[0m"
    exit
}

usage() {
    printf "Usage: %s [[-]r|[--]run]  [[-]h|[--]help ]\n" "$(basename "$0")"
    echo "  -i, --info    show info about this script and exits"
    echo "  -r, --run     run the installation"
    echo "   *            print this menu"
    exit
}

install_nixos() {
    # Check if the user which run the script is root
    if [[ $EUID -ne 0 ]]; then
	      log E "This scripts needs to be runned as root!"
	      exit 1
    fi

    log I "Select the device to use to install NixOS:"

    # Select device on which to install NixOS
    local device
    if ! device=$(find /dev/ -regex "/dev/\(sd[a-z]\|vd[a-z]\|nvme[0-9]n[0-9]\)" | fzf); then
        log E "Invalid device selected! Exiting..."
        exit 1
    fi

    printf "\e[32m%b\e[0m" "This device will be wiped, are you sure you want to use this device? [y/N] "
    read -r sure
    [ "$sure" != 'y' ] && exit 1

    sgdisk -p "$device"

    log I "Wiping out all the data and signatures of the choosen disk (${device})...\n"

    # Check if the current device selected is an nvme
    is_nvme='' && [[ "$device" =~ /dev/nvme.* ]] && is_nvme='p'

    # Define partition name based on the disk (nvme/sda...)
    boot_device="${device}${is_nvme}1"
    swap_device="${device}${is_nvme}2"
    root_device="${device}${is_nvme}3"
    
    wipefs --all -f "${device}"
    sgdisk --zap-all "${device}"

    log I "Done"
    log I "Starting partitioning of ${device}..."

    # MEMO
    # -n <a>:<b>:<c>: New partition with number a, starting at sector b and ending at sector c.
    # Note that specifying a partition number of 0 always takes the first available number
    # -t (partition type code):
    #    EF00 -> boot efi
    #    8200 -> swap
    #    8300 -> Linux Filesystem / 8303 -> Linux x86 root (/)
    # To get a list of available typecode use `sgdisk -L`

    sgdisk -n 1:0:+512M -t 1:EF00 "${device}"
    sgdisk -n 2:0:+20G -t 2:8200 -c 2:swap "${device}"
    sgdisk -n 3:0:0 -t 3:8300 -c 3:root "${device}"

    # force re-reading the partition table
    sync
    partprobe "$device"

    # print results
    sgdisk -p "$device"

    log I "Starting cryptsetup...\n"
    log I "Setting luks2 partition\n"
    cryptsetup --type luks2 luksFormat "$root_device"
    cryptsetup open "$root_device" nix-enc
    cryptsetup config "$root_device" --label nix-enc
    root="/dev/mapper/nix-enc"


    log I "Formatting partitions...\n"
    mkfs.vfat -n boot "$boot_device"

    ### SWAP ###
    mkswap "$swap_device" -L SWAP
    swapon "$swap_device"
    mkfs.btrfs -L vault -f --checksum xxhash "$root"  # -> vault, blackhole

    log I "Done!"

    mount -t btrfs "$root" /mnt

    # We first create the subvolumes outlined above:
    for sv in "${subvolumes_list[@]}"; do
        log I "Creating volume /mnt/$sv..."
        btrfs subvolume create "/mnt/$sv"
    done

    # We then take an empty *readonly* snapshot of the root subvolume,
    # which we'll eventually rollback to on every boot.
    log I "Taking a blank snapshot..."
    btrfs subvolume snapshot -r "/mnt/@" "/mnt/root-blank" # Alternative "/mnt/erase"

    umount /mnt

    log I "Mounting subvolumes..."
    mount -o subvol=@,"${subvolumes_flags[*]}" "$root" /mnt

    log I "Creating directories..."

    mkdir -p /mnt/{boot,home,nix,persist,var/log}

    mount -o subvol=@home,"${subvolumes_flags[*]}" "$root" /mnt/home
    mount -o subvol=@nix,"${subvolumes_flags[*]}" "$root" /mnt/nix
    mount -o subvol=@persist,"${subvolumes_flags[*]}" "$root" /mnt/persist
    mount -o subvol=@var_log,"${subvolumes_flags[*]}" "$root" /mnt/var/log

    mount "$boot_device" "/mnt/boot"

    # Generate the basic configuration files
    nixos-generate-config --root /mnt
}

case "$1" in
	  -i|--info) info ;;
	  -r|--run) install_nixos ;;
	  *) usage ;;
esac
