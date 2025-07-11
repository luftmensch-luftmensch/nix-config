_: {
  system.modules = {
    core = {
      boot = {
        luks = {
          enable = true;
          keyFile = "/dev/disk/by-id/usb-Generic_Flash_Disk_DA9B8538-0:0";
        };
        quietboot.enable = true;
      };
      cachix.enable = true;
      environment.enable = true;
      impermanence.enable = true;
      user = {
        enable = true;
        username = "valentino";
        description = "Valentino Bocchetti";
        hashedPassword = "$6$4CvDCvwb2FE/EnAv$pP65k96pPwwOVu1aspgUbYSwq9HUgwzFCdvOvHq/rj6te1KdFNAWHbo65aql15awsUrKcianHFSTm5GzqA.MZ.";
        extraAuthorizedKeys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO7aU2VLocgIoOxTFPkq+nfbQlhOhicbNSW4mDfuj4vL valentino@homelab"
        ];
        extraGroups = [ "dialout" ]; # support for serial device
      };
      slim.enable = true;
    };

    credentials.ssh.enable = true;

    dev = {
      adb.enable = true;
      docker.enable = true;
      keyboard.enable = true;
      manpages.enable = true;
    };

    graphical = {
      xorg.enable = true;
      sddm.enable = true;
    };

    hardware = {
      audio.enable = true;
      video.i2c.enable = true;
      bluetooth.enable = true;
    };

    services = {
      printing = {
        cups.enable = true;
        sane.enable = true;
      };
      udev-rules.enable = true;
      syncthing = {
        enable = true;
        id = "7V7Y5YT-MOLUCYZ-434QHLQ-BJDAMRJ-Z2PDSSG-XKAQX4M-W3CONND-LHDWQQB";
      };
    };

    vpn.openconnect.enable = true;

    shell.bash.enable = true;
  };
}
