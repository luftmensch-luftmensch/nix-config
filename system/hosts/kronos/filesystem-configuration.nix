{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  boot = {
    initrd = {
      luks.devices."nix-enc" = {
        device = "/dev/disk/by-label/nix-enc";
        preLVM = true;
        allowDiscards = true;
        keyFileSize = 4096;
        keyFile = "/dev/disk/by-id/usb-Innostor_Innostor_ALF3DAEARV00000001519-0:0";
        fallbackToPassword = true;
      };
    };
  };
}
