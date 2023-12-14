{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.emacs;
  configDir = "${config.home.homeDirectory}/nix-config/home/valentino/modules/editors/emacs/config";
in {
  options.valentino.modules.editors.emacs = {
    enable = mkEnableOption "emacs and its configuration";
    package = mkOption {
      type = types.package;
      default = "default";
      defaultText = "default";
      description = "Which emacs package to use";
    };
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
        # package = pkgs.emacs29-pgtk;
        package = cfg.package;

        extraPackages = epkgs:
          with epkgs; [
            vterm
            pdf-tools
            treesit-grammars.with-all-grammars
          ]
          ++ (optionals cfg.telega.enable [
            # epkgs.melpaPackages.telega
            # melpaPackages.telega is outdated. Pull a newer version directly from the repo
            (epkgs.melpaPackages.telega.overrideAttrs (oldAttrs: {
              version = "0.8.220";
              src = pkgs.fetchFromGitHub {
                owner = "zevlg";
                repo = "telega.el";
								rev = "e0ad17b5650b98313219ece3fc371ec051f7a597";
								sha256 = "049xv1ysg0r46k47z3dkdkwqh1f086c5l9yp7c9cs45vg8cj283x";
              };
            }))

          ]);
      };

      # xdg.configFile."emacs/Emacs.org".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/Emacs.org";
      xdg.configFile."emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/init.el";
      xdg.configFile."emacs/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/early-init.el";
      xdg.configFile."emacs/lisp".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/lisp";
      # xdg.configFile."emacs/straight/versions".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/straight/versions";
      # xdg.configFile."emacs/img".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/img";
    }

    (mkIf cfg.daemon.enable {
      services.emacs = {
        enable = true;
        client.enable = true;
        startWithUserSession = "graphical";
      };

      # home.sessionVariables = {
      #   EDITOR = "emacsclient -t";
      #   VISUAL = "emacsclient -c -a emacs";
      # };
    })

    (mkIf config.programs.notmuch.enable {
      home.packages = with pkgs; [
        notmuch.emacs
      ];
    })
  ]);
}
