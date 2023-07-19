#!/usr/bin/env bash
set -e # exit immediately if a command return non-zero status

# Global variables
declare -a subvolumes_list=("@" "@home" "@nix" "@persist" "@var_log")
declare -a subvolumes_flags=("compress=zstd:3" "noatime" "autodefrag" "space_cache=v2" "discard=async")

print_ok() {
    printf "\e[32m%b\e[0m" "$1"
}

print_info() {
    printf "\e[36m%b\e[0m" "$1"
}

print_error() {
    printf "\e[31m%b\e[0m" "$1"
}


usage() {

  printf "Usage: %s [[-]f|[--]fix ] [[-]r|[--]run]  [[-]h|[--]help ]\n" "$(basename "$0")"
  echo "  -h, --help    prints this message and exits"
  echo "  -r, --run     run the installation"
  echo "   *            print this menu"
}

print_help() {
    usage

    print_ok "This script generate a full working Nixos installation with encrypted BTRFS partition as root\n"
    print_ok "This script will wipe the given device, these are the partitions that will be created\n"

    print_info "1: boot partition\n"

    printf "\e[32m"
    printf "\t%s\n" "FS: vfat" "Size: 1024M" "Mount Point: /boot"

    print_info "2: luks2 encrypted partition (when enabled)\n"

    printf "\e[32m"

    printf "\t%s\n" "Mount Point: /dev/mapper/nix-enc"
    printf "\t\t%s\n" "2.1: root partition"
    printf "\t\t\t%s\n" "FS: btrfs" "Size: rest of the disk" "Mount Point: none" "Mount Options: autodefrag,space_cache=v2,noatime,compress=zstd:3"
    printf "\t\t\t\t\t%s\n" "'discard=async' will be added when ssd is detected"
    printf "\t\t\t%s\n" "Subvolumes:"
    printf "\t\t\t\tSubvolumes \t\t\t: Mount Point \t\t\t:Specific flags\n"

    # Lista dei subvolumes + mount point + flags 
    printf "\t\t\t\t@ \t\t\t\t: / \t\t\t\t%s\n" "${subvolumes_flags[*]}"
    printf "\t\t\t\t@home \t\t\t\t: /home \t\t\t%s\n" "${subvolumes_flags[*]}"
    printf "\t\t\t\t@nix \t\t\t\t: /nix \t\t\t\t%s\n" "${subvolumes_flags[*]}"
    printf "\t\t\t\t@persist \t\t\t: /persist \t\t\t%s\n" "${subvolumes_flags[*]}"
    printf "\t\t\t\t@var_log \t\t\t: /var/log \t\t\t%s\n" "${subvolumes_flags[*]}"
    printf "\e[0m"
    exit
}

fail (){
  printf ''
  print_info 'Incorrect arguments, did you make a typo?\n'
  printf ''
  print_info "Usage: $(basename "$0") [[-]r|[--]run] [[-]h|[--]help ] \n"
  print_error "This script wipes all your data to  generate a full working\nNixos installation with encrypted BTRFS partition as root\n"
  print_error "\t\t\tTake care!\n"
  printf ''
  exit 1
}

install_nixos(){
    # Check if the user which run the script is root
    if [[ $EUID -ne 0 ]]; then
	print_error "This scripts needs to be runned as root!"
	exit 1
    fi

    # Search device on which to install NixOS
    
    print_info "Available devices:\n\t$(find /dev/ -regex "/dev/\(sd[a-z]\|vd[a-z]\|nvme[0-9]n[0-9]\)" | tr '\n' ' ')\n" # Print all the available devices on the same line

    # loop as long as $device is a valid device
    while [ -z "$device" ] || [ ! -e "$device" ] || \
	      ! expr "$device" : '^/dev/\(sd[a-z]\|vd[a-z]\|nvme[0-9]n[0-9]\)$' >/dev/null; do
	print_info "Type the device name ('/dev/' required): "
	read -r device
	[ ! -e "$device" ] && print_error "This device doesn't exist\n"
	if ! expr "$device" : '^/dev/\(sd[a-z]\|vd[a-z]\|nvme[0-9]n[0-9]\)$' >/dev/null; then
	    print_error "You should type a device name, not a partition name\n"
	    device=""
	fi
    done

    sgdisk -p "$device"
    print_info "This device will be wiped, are you sure you want to use this device? [y/N] "
    read -r sure
    [ "$sure" != 'y' ] && exit 1

    print_error "Wiping out all the data and signatures of the choosen disk (${device})...\n"

    # Define partition name based on the disk (nvme/sda...)
    if echo "$device" | grep -q "nvme"; then
	print_info "Detected nvme...\n"
	boot_device="${device}p1"
	swap_device="${device}p2"
	root_device="${device}p3"
    else
	boot_device="${device}1"
	swap_device="${device}2"
	root_device="${device}3"
    fi

    wipefs --all -f "${device}"
    sgdisk --zap-all "${device}"

    print_info "Done!\n"
    print_info "Starting partitioning of ${device} ..."

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
    sgdisk -n 3:0:0 -t 3:8300 -c 3:root "${device}"  # Update to 3 when swap is enabled

    # force re-reading the partition table
    sync
    partprobe "$device"

    # print results
    sgdisk -p "$device"

    print_ok "Starting cryptsetup...\n"
    print_info "Setting luks2 partition\n"
    cryptsetup --type luks2 luksFormat "$root_device"
    cryptsetup open "$root_device" nix-enc
    cryptsetup config "$root_device" --label nix-enc
    root="/dev/mapper/nix-enc"


    print_ok "Formatting partitions...\n"
    mkfs.vfat -n boot "$boot_device"

    ### SWAP ###
    mkswap "$swap_device" -L SWAP
    swapon "$swap_device"
    mkfs.btrfs -L vault -f --checksum xxhash "$root"  # -> vault, blackhole

    print_info "Done!\n"

    mount -t btrfs "$root" /mnt

    # We first create the subvolumes outlined above:
    for sv in "${subvolumes_list[@]}";
    do
	btrfs subvolume create "/mnt/$sv"
    
    done
    # We then take an empty *readonly* snapshot of the root subvolume,
    # which we'll eventually rollback to on every boot.
    print_info "Taking a blank snapshot...\n"
    btrfs subvolume snapshot -r "/mnt/@" "/mnt/root-blank" # Alternative "/mnt/erase"

    umount /mnt

    print_info "Mounting subvolumes...\n"
    mount -o subvol=@,"${subvolumes_flags[*]}" "$root" /mnt

    print_info "Creating directories...\n"

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
	-h|--help) print_help ;;

	-r|--run) install_nixos ;;
	*) fail ;;
esac
