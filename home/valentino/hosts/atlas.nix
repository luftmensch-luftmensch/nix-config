{ pkgs, ... }:
{
  valentino.modules = {
    apps = {
      rofi.enable = true;
      outlook.enable = true;
      teams.enable = true;
      vnc.remmina.enable = true;
    };

    xorg.enable = true;

    browsers = {
      firefox.enable = true;
      chromium.enable = true;
    };

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
      mail-defaults.enable = true;
      _1password.enable = true;
      proton = {
        authenticator.enable = true;
        pass.enable = true;
      };
    };

    dev = {
      android.enable = true;
      cc.enable = true;
      java.enable = true;
      nix.enable = true;
      python.enable = true;
      tex.enable = true;
      tools.enable = true;
    };

    editors = {
      emacs = {
        enable = true;
        daemon.enable = true;
        telega.enable = true;
        orgSetupFiles.enable = true;
      };
      neovim.enable = true;
    };

    media = {
      documents.enable = true;
      reading.mdx.enable = true;
      editing.enable = true;
      files = {
        filezilla.enable = true;
        gallery-dl.enable = true;
        libreoffice.enable = true;
        localsend.enable = true;
        nemo.enable = true;
        qrcp = {
          enable = true;
          interface = "enp2s0";
        };
      };
      images = {
        imv.enable = true;
        feh.enable = true;
      };
      music.enable = true;
      videos.enable = true;
    };

    term = {
      alacritty.enable = true;
      kitty.enable = true;
    };

    shell = {
      bash.enable = true;
      extensions.enable = true;
      fish = {
        enable = true;
        cpuTuning = true;
      };
      direnv.enable = true;
      git = {
        enable = true;
        gh.enable = true;
      };
      nix-index.enable = true;
    };

    themes = {
      active = "modus";
      darkTheme = true;

      font = {
        regular = {
          family = "Sarasa Mono Slab SC";
          package = pkgs.sarasa-gothic;
          size = 12;
        };

        monospace = {
          family = "FiraCode Nerd Font";
          package = pkgs.nerd-fonts.fira-code;
          size = 12;
        };

        term = {
          family = "VictorMono Nerd Font";
          package = pkgs.nerd-fonts.victor-mono;
          size = 12;
        };

        bar = {
          family = "Iosevka Nerd Font";
          package = pkgs.nerd-fonts.iosevka;
          size = 12;
        };
      };
    };
  };
}
