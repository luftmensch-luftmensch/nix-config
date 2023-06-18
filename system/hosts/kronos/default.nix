{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./filesystem-configuration.nix
    ./options.nix
    ../common.nix
  ];

  # Kernel related
  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    kernelModules = ["kvm-amd" "amdgpu" "acpi_call"];
    kernelParams = ["acpi_backlight=native" "idle=nomwait" "iommu=pt"];
    extraModulePackages = with config.boot.kernelPackages; [acpi_call];
    supportedFilesystems = ["btrfs"];
  };

  # Networking specific
  networking = {
    hostId = builtins.substring 0 8 (builtins.hashString "md5" config.networking.hostName);
    useDHCP = false; # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    interfaces = {
      enp2s0f0.useDHCP = true;
      wlp3s0.useDHCP = true;
    };
  };

  # Hardware specifics
  hardware = {
    cpu = {
      amd.updateMicrocode = true; # lib.mkDefault config.hardware.enableRedistributableFirmware;
    };

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [amdvlk vaapiVdpau libvdpau-va-gl];
    };
    # In substitution of nixos-hardware
    firmware = [pkgs.linux-firmware];

    trackpoint = {
      enable = lib.mkDefault true;
      emulateWheel = lib.mkDefault true;
    };
  };

  services.xserver = {
    enable = true;
    videoDrivers = ["amdgpu"];
  };
}
