{pkgs, ...}: {
  valentino.modules = {
    apps = {
      api_tools.enable = true;
      rofi.enable = true;
    };

    xorg = {
      enable = true;
      wm = "i3";
    };

    browsers = {
      firefox.enable = true;
      chromium.enable = true;
    };

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
      mail-defaults.enable = true;
      bitwarden.enable = true;
    };

    dev = {
      cc.enable = true;
      generics.enable = true;
      java.enable = true;
      js.enable = true;
      nix.enable = true;
      python.enable = true;
      tex.enable = true;
    };

    editors = {
      intellij.enable = true;
      vscode.enable = true;
    };

    media = {
      documents = {
        zathura.enable = true;
      };
      editing.enable = true;
      files = {
        filezilla.enable = true;
        libreoffice.enable = true;
        localsend.enable = true;
        qrcp = {
          enable = true;
          interface = "enp2s0";
        };
      };
      images = {
        imv = {
          enable = true;
          setWallpaper = "feh --bg-scale";
        };
        feh.enable = true;
      };
      music.enable = true;
      videos.enable = true;
    };

    term = {
      alacritty.enable = true;
    };

    shell = {
      bash.enable = true;
      extensions.enable = true;
      fish = {
        enable = true;
        cpuTuning = true;
      };
      tmux.enable = true;
      direnv.enable = true;
      git.enable = true;
    };

    themes = {
      active = "modus";
      darkTheme = true;

      font = {
        regular = {
          family = "Roboto";
          package = pkgs.roboto;
          size = 12;
        };

        monospace = {
          family = "FiraCode Nerd Font";
          package = pkgs.nerdfonts.override {fonts = ["FiraCode"];};
          size = 12;
        };

        term = {
          family = "VictorMono Nerd Font";
          package = pkgs.nerdfonts.override {fonts = ["VictorMono"];};
          size = 12;
        };

        bar = {
          family = "Iosevka Nerd Font";
          package = pkgs.nerdfonts.override {fonts = ["Iosevka"];};
          size = 12;
        };
      };
    };
  };
}