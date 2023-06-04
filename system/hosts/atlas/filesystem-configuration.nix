{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  
  boot = {
    initrd = {
      luks.devices."nix-enc" = {
        device = "/dev/disk/by-label/nix-enc";      
        preLVM = true;
        allowDiscards = true;  
        keyFileSize = 4096;
        keyFile = "/dev/disk/by-id/usb-Generic_Flash_Disk_DA9B8538-0:0";
        fallbackToPassword = true;
      };
    };
  };
}
