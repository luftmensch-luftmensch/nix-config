{
  pkgs,
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    ./options.nix
    ../common.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Kernel related
  boot = {
    kernelPackages = pkgs.linuxPackages;
    kernelModules = ["kvm-intel"];
    supportedFilesystems = ["btrfs"];
  };

  # Networking specific
  networking = {
    hostId = builtins.substring 0 8 (builtins.hashString "md5" config.networking.hostName);
    useDHCP = false; # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    interfaces.enp2s0.useDHCP = true;
  };

  # Hardware specifics
  hardware = {
    cpu = {
      intel.updateMicrocode = true; # lib.mkDefault config.hardware.enableRedistributableFirmware;
    };

    # nvidia = {
    #   package = config.boot.kernelPackages.nvidiaPackages.legacy_470; # beta
    #   modesetting.enable = true;
    # };
  };

  services.xserver = {
    enable = true;
    videoDrivers = ["intel"]; # "nvidia"
  };
}
