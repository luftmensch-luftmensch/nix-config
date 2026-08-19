{
  pkgs,
  config,
  modulesPath,
  ...
}:
{
  imports = [
    ./options.nix
    ../common.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Kernel related
  boot = {
    kernelPackages = pkgs.linuxPackages;
    kernelModules = [ "kvm-intel" ];
    supportedFilesystems = [ "btrfs" ];
  };

  # Networking specific
  networking = {
    hostId = builtins.substring 0 8 (builtins.hashString "md5" config.networking.hostName);
    interfaces.enp2s0.useDHCP = true;
  };

  # Hardware specifics
  hardware = {
    cpu.intel.updateMicrocode = true; # lib.mkDefault config.hardware.enableRedistributableFirmware;

    # nvidia = {
    #   package = config.boot.kernelPackages.nvidiaPackages.legacy_470; # beta
    #   modesetting.enable = true;
    # };
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "modesetting" ]; # intel nvidia
    # 120Hz supported, might as well use it
    monitorSection = ''
      Option "PreferredMode" "1920x1080_120"
    '';

    screenSection = ''
      Option "metamodes" "1920x1080_120 +0+0"
    '';
  };
}
