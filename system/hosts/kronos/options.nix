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
        extraGroups = ["networkmanager" "docker" "scanner" "lp" "lpadmin" "adbusers" "plugdev" "input"];
        extraAuthorizedKeys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILKiPJdb2rsLRwFGczP8x7KVTjqqY4lPkdbwE6+GcKxP valentino@homelab"];
      };
    };

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
    };

    dev = {
      adb.enable = true;
      docker.enable = true;
      manpages.enable = true;
      # virtualisation.enable = true;
    };

    editor = {
      emacs = {
        enable = true;
        package = pkgs.emacs29-pgtk;
        daemon.enable = true;
        mails.enable = true;
        enableSpelling = true;
      };
      ide.enable = true;
      neovim.enable = true;
    };

    graphical = {
      wayland.enable = true;
      sddm.enable = true;
    };

    hardware = {
      audio = {
        enable = true;
        enablePipewire = true;
      };
      bluetooth.enable = true;
    };

    packages = {
      base.enable = true;
      miscellaneous.enable = true;
      programming = {
        c-packages = true;
        nix-packages = true;
        java-packages = true;
        # go-packages = true;
        python-packages = true;
        misc-packages = true;
      };

      ricing.enable = true;
      secrets.enable = true;
      # unstable.enable = true;
      utilities.enable = true;
    };

    services = {
      battery.enable = true;
      fingerprint.enable = true;
      logind.enable = true;
      printing = {
        enableCups = true;
      };
      udev-rules.enable = true;
      syncthing = {
        enable = true;
        device-id = "6LMWCO4-HGJ5WVU-W3CKJYV-GLBR2KF-IMB6E7A-NKLI7ZF-SZ7N4YX-YBE26AN";
      };
      touchpad.enable = true;
    };

    vpn = {
      openvpn.enable = true;
      wireguard.enable = true;
    };


    shell = {
      bash.enable = true;
    };
  };
}
