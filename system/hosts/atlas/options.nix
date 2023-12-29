{pkgs, ...}: {
  system.modules = {
    core = {
      boot.quietboot.enable = true;
      cachix.enable = true;
      environment.enable = true;
      impermanence.enable = true;
      user = {
        enable = true;
        username = "valentino";
        description = "Valentino Bocchetti";
        hashedPassword = "$6$4CvDCvwb2FE/EnAv$pP65k96pPwwOVu1aspgUbYSwq9HUgwzFCdvOvHq/rj6te1KdFNAWHbo65aql15awsUrKcianHFSTm5GzqA.MZ.";
        extraGroups = ["networkmanager" "docker" "scanner" "lp" "lpadmin" "adbusers" "plugdev" "libvirtd"];
        extraAuthorizedKeys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO7aU2VLocgIoOxTFPkq+nfbQlhOhicbNSW4mDfuj4vL valentino@homelab"];
      };
    };

    credentials = {
      ssh.enable = true;
      # gpg.enable = true;
    };

    dev = {
      adb.enable = true;
      docker.enable = true;
      manpages.enable = true;
      virtualisation.enable = true;
    };

    editor = {
      # emacs = {
      #   enable = true;
      #   package = pkgs.emacs29-gtk3;
      #   daemon.enable = true;
      #   mails.enable = true;
      #   enableSpelling = true;
      # };
      neovim.enable = true;
    };

    graphical = {
      xorg.enable = true;
      sddm.enable = true;
    };

    hardware = {
      audio = {
        enable = true;
        enablePulseaudio = true;
      };
      bluetooth.enable = true;
    };

    packages = {
      ricing.enable = true;
    };

    services = {
      printing = {
        enableCups = true;
        # enableSane = true;
      };
      udev-rules.enable = true;
      syncthing = {
        enable = true;
        device-id = "7V7Y5YT-MOLUCYZ-434QHLQ-BJDAMRJ-Z2PDSSG-XKAQX4M-W3CONND-LHDWQQB";
      };
    };

    vpn = {
      openvpn.enable = true;
    };

    shell = {
      bash.enable = true;
      # fish.enable = true;
    };
  };
}
