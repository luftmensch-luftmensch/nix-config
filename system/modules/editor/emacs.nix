{
  config,
  lib,
  pkgs,
  # inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.editor.emacs;
in {
  options.system.modules.editor.emacs = {
    enable = mkEnableOption "Enable emacs";
    package = mkOption {
      type = types.package;
      default = "default";
      defaultText = "default";
      description = "Which emacs package to use";
    };
    daemon.enable = mkEnableOption "Enable daemon";
    mails.enable = mkEnableOption "Enable emacs mails capabilities";
    enableSpelling = mkEnableOption "Enable emacs spelling capabilities";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # nixpkgs.overlays = [inputs.emacs-overlay.overlays.default];

      environment.systemPackages = with pkgs; [
        ((emacsPackagesFor cfg.package).emacsWithPackages (epkgs:
          with epkgs; [
            # melpaPackages.telega is outdated. Pull a newer version directly from the repo
            (melpaPackages.telega.overrideAttrs (oldAttrs: {
              version = "0.8.220";
              src = pkgs.fetchFromGitHub {
                owner = "zevlg";
                repo = "telega.el";
								rev = "e0ad17b5650b98313219ece3fc371ec051f7a597";
								sha256 = "049xv1ysg0r46k47z3dkdkwqh1f086c5l9yp7c9cs45vg8cj283x";
              };
            }))
            vterm
            pdf-tools
            treesit-grammars.with-all-grammars
            auctex
          ]))
        exiftool
        mupdf
        (writeScriptBin "emacs-daemon" ''
          emacs --fg-daemon
        '')
      ];

      # Install emacs icons symbols if we have any kind of graphical emacs
      fonts.packages = with pkgs; [emacs-all-the-icons-fonts];
    }
    (mkIf cfg.mails.enable {
      environment.systemPackages = with pkgs; [afew isync libsecret mu notmuch notmuch.emacs thunderbird];
      programs.msmtp = {
        enable = true;
      };
    })

    (mkIf cfg.enableSpelling {
      environment.systemPackages = with pkgs; [hunspell hunspellDicts.it_IT hunspellDicts.en_US];
    })
  ]);
}
