_: {
  system.modules = {
    core = {
      boot = {
        luks = {
          enable = true;
          keyFile = "/dev/disk/by-id/usb-Innostor_Innostor_ALF3DAEARV00000001519-0:0";
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
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILKiPJdb2rsLRwFGczP8x7KVTjqqY4lPkdbwE6+GcKxP valentino@homelab"
        ];
      };
      slim.enable = true;
    };

    credentials.ssh.enable = true;

    dev = {
      adb.enable = true;
      docker.enable = true;
      manpages.enable = true;
    };

    graphical = {
      wayland.enable = true;
      sddm.enable = true;
    };

    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
    };

    services = {
      battery.enable = true;
      fingerprint.enable = true;
      logind.enable = true;
      printing.cups.enable = true;
      udev-rules.enable = true;
      syncthing = {
        enable = true;
        id = "6LMWCO4-HGJ5WVU-W3CKJYV-GLBR2KF-IMB6E7A-NKLI7ZF-SZ7N4YX-YBE26AN";
      };
      touchpad.enable = true;
    };

    vpn = {
      openconnect.enable = true;
      wireguard.enable = true;
    };

    shell.bash.enable = true;
  };
}
