{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.emacs;
  cfgXorg = config.valentino.modules.xorg;
  configDir = "${config.home.homeDirectory}/nix-config/home/valentino/modules/editors/emacs/config";
in {
  options.valentino.modules.editors.emacs = {
    enable = mkEnableOption "emacs and its configuration";
    daemon.enable = mkEnableOption "emacs daemon";
    telega.enable = mkEnableOption "telegram client";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        hack-font

        hunspell
        hunspellDicts.it_IT
        hunspellDicts.en_US
      ];

      programs.emacs = {
        enable = true;
        package =
          if cfgXorg.enable
          then pkgs.emacs29-gtk3
          else pkgs.emacs29-pgtk;

        extraPackages = epkgs:
          with epkgs; [
            auctex
            pdf-tools
            treesit-grammars.with-all-grammars
            vterm

            (melpaPackages.telega.overrideAttrs (oldAttrs: {
              version = "0.8.220";
              src = pkgs.fetchFromGitHub {
                owner = "zevlg";
                repo = "telega.el";
                rev = "c522d366aebcdf2178c34978f7f5d3167840a641";
                sha256 = "0vsdcdj3b9sbmjcihd4zqfdnspdjh2806c77kzsl62jpyj55zkj4";
              };
            }))
          ];
        # ++ (optionals cfg.telega.enable [
        #   # melpaPackages.telega is outdated. Pull a newer version directly from the repo
        #   # (melpaPackages.telega.overrideAttrs (oldAttrs: {
        #   #   version = "0.8.220";
        #   #   src = pkgs.fetchFromGitHub {
        #   #     owner = "zevlg";
        #   #     repo = "telega.el";
        #   #     rev = "c522d366aebcdf2178c34978f7f5d3167840a641";
        #   #     sha256 = "0vsdcdj3b9sbmjcihd4zqfdnspdjh2806c77kzsl62jpyj55zkj4";
        #   #   };
        #   # }))
        #   telega
        # ]);
      };

      xdg.configFile = {
        "emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/init.el";
        "emacs/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/early-init.el";
        "emacs/lisp".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/lisp";
        "emacs/bookmarks".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/bookmarks";

        # "emacs/Emacs.org".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/Emacs.org";
        # "emacs/straight/versions".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/straight/versions";
      };
    }

    (mkIf cfg.daemon.enable {
      services.emacs = {
        enable = true;
        client.enable = true;
        startWithUserSession = "graphical";
      };

      home.sessionVariables = {
        # EDITOR = "emacsclient -t";
        # VISUAL = "emacsclient -c -a emacs";
      };
    })

    (mkIf config.programs.notmuch.enable {
      home.packages = [pkgs.notmuch.emacs];
    })

    (mkIf cfg.telega.enable {
      # Cross-platform library for building Telegram clients
      home.packages = [pkgs.tdlib];
    })
  ]);
}
