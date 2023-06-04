{
  pkgs,
  ...
}: {
  
  system.modules = {
    core = {
      boot.quietboot.enable = true;
      cachix.enable         = true;
      environment.enable    = true;
      impermanence.enable   = true;
      user = {
        enable              = true;
        username            = "valentino";
        description         = "Valentino Bocchetti";
        hashedPassword      = "$6$4CvDCvwb2FE/EnAv$pP65k96pPwwOVu1aspgUbYSwq9HUgwzFCdvOvHq/rj6te1KdFNAWHbo65aql15awsUrKcianHFSTm5GzqA.MZ.";
        extraGroups         = [ "networkmanager" "docker" "scanner" "lp" "lpadmin" "adbusers" "input" ];
        extraAuthorizedKeys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDLhTJTjWwzsg+e3xwCJ4yweI064n1MYtgT4V8ksBYKn valentino@homelab" ];
      };
    };

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
    };

    dev = {
      adb.enable            = true;
      docker.enable         = true;
      manpages.enable       = true;
      # virtualisation.enable = true;
    };

    editor = {
      emacs = {
        enable         = true;
        package        = pkgs.emacs29-pgtk;
        daemon.enable  = true;
        mails.enable   = true;
        enableSpelling = true;
      };
      ide.enable    = true;
      neovim.enable = true;
    };

    graphical = {
      wayland.enable = true;
      sddm.enable    = true;
    };

    hardware = {
      audio = {
        enable         = true;
        enablePipewire = true;
      };
      bluetooth.enable = true;
    };

    packages = {
      base.enable          = true;
      miscellaneous.enable = true;
      programming = {
        c-packages      = true;
        nix-packages    = true;
        java-packages   = true;
        go-packages     = true;
        python-packages = true;
        misc-packages   = true;
      };

      ricing.enable    = true;
      secrets.enable   = true;
      utilities.enable = true;
    };

    services = {
      battery.enable    = true;
      logind.enable     = true;
      printing = {
        enableCups = true;
      };
      udev-rules.enable = true;
      syncthing = {
        enable    = true;
        device-id = "LXTK5LZ-CT2L6QP-MTAGSYX-XIRR2N2-GQOB7XS-LQCRUTS-UDB36R5-O3OEXQ2";
      };
      touchpad.enable  = true;
      wireguard.enable = true;
    };

    shell = {
      bash.enable = true;
    };
  };
}
