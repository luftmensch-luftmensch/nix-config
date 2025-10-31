{ pkgs, ... }:
{
  valentino.modules = {
    apps = {
      rofi.enable = true;
      outlook.enable = true;
      teams.enable = true;
      vnc = {
        anydesk.enable = true;
        remmina.enable = true;
        horizon.enable = true;
      };
    };

    wayland.enable = true;

    browsers = {
      firefox.enable = true;
      chromium.enable = true;
    };

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
      mail-defaults.enable = true;
      bitwarden.enable = true;
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
      js.enable = true;
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
        localsend.enable = true;
        nemo.enable = true;
        qrcp = {
          enable = true;
          interface = "wlp3s0";
        };
      };
      images.imv.enable = true;
      music.enable = true;
      videos.enable = true;
    };

    term.foot.enable = true;

    shell = {
      bash.enable = true;
      extensions.enable = true;
      fish.enable = true;
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
          size = 14;
        };

        monospace = {
          family = "FiraCode Nerd Font";
          package = pkgs.nerd-fonts.fira-code;
          size = 14;
        };

        term = {
          family = "VictorMono Nerd Font";
          package = pkgs.nerd-fonts.victor-mono;
          size = 14;
        };

        bar = {
          family = "Iosevka Nerd Font";
          package = pkgs.nerd-fonts.iosevka;
          size = 14;
        };
      };
    };
  };
}
