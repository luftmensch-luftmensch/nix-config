{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.emacs;
  configDir = "${config.home.homeDirectory}/nix-config/home/valentino/modules/editors/emacs/config";
  inherit (config.valentino.modules) xorg;
in {
  options.valentino.modules.editors.emacs = {
    enable = mkEnableOption "emacs and its configuration";
    daemon.enable = mkEnableOption "emacs daemon";
    telega.enable = mkEnableOption "telegram client";
    orgSetupFiles.enable = mkEnableOption "custom classes for org writing";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [hunspell hunspellDicts.it_IT hunspellDicts.en_US];

      programs.emacs = {
        enable = true;
        package =
          if xorg.enable
          then pkgs.emacs29-gtk3
          else pkgs.emacs29-pgtk;

        extraPackages = epkgs:
          with epkgs;
            [
              auctex
              jinx
              pdf-tools
              treesit-grammars.with-all-grammars
              vterm
            ]
            ++ (optionals cfg.telega.enable [
              # melpaPackages.telega is outdated. Pull a newer version directly from the repo
              (melpaPackages.telega.overrideAttrs (_oldAttrs: {
                version = "0.8.250";
                src = pkgs.fetchFromGitHub {
                  owner = "zevlg";
                  repo = "telega.el";
                  rev = "d1cf1ffe289a18f366b7f3b64f827f0d0755947a";
                  sha256 = "0s1w4zb252d70n05dbsv2rnlfsg53paklafqxasl566nnzbi0d2q";
                };
              }))
            ]);
      };

      xdg.configFile = {
        "emacs/setup.org".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/setup.org";
        "emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/init.el";
        "emacs/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/early-init.el";
        "emacs/lisp".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/lisp";
        "emacs/bookmarks".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/bookmarks";
      };
    }

    (mkIf cfg.orgSetupFiles.enable {
      xdg.configFile."emacs/setup_files".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/setup_files";
    })

    (mkIf cfg.daemon.enable {
      services.emacs = {
        enable = true;
        client.enable = true;
        startWithUserSession = "graphical";
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
