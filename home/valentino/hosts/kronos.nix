{ pkgs, ... }:
{
  valentino.modules = {
    apps = {
      rofi.enable = true;
      vnc.enable = true;
    };

    wayland = {
      enable = true;
      wm = [ "sway" ];
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
      _1password.enable = true;
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
      intellij.enable = true;
      neovim.enable = true;
      vscode.enable = true;
    };

    media = {
      documents.zathura.enable = true;
      reading.mdx.enable = true;
      editing.enable = true;
      files = {
        filezilla.enable = true;
        localsend.enable = true;
        qrcp = {
          enable = true;
          interface = "wlp3s0";
        };
      };
      images.imv.enable = true;
      music.enable = true;
      videos.enable = true;
    };

    term = {
      alacritty.enable = true;
      foot.enable = true;
    };

    shell = {
      bash.enable = true;
      extensions.enable = true;
      fish.enable = true;
      direnv.enable = true;
      git.enable = true;
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
          package = pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; };
          size = 14;
        };

        term = {
          family = "VictorMono Nerd Font";
          package = pkgs.nerdfonts.override { fonts = [ "VictorMono" ]; };
          size = 14;
        };

        bar = {
          family = "Iosevka Nerd Font";
          package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
          size = 14;
        };
      };
    };
  };
}
