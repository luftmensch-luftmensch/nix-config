{
  config,
  lib,
  pkgs,
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
      environment.systemPackages = with pkgs; [
        ((emacsPackagesFor cfg.package).emacsWithPackages (epkgs:
          with epkgs; [
            # melpaPackages.telega is outdated. Pull a newer version directly from the repo
            (melpaPackages.telega.overrideAttrs (_oldAttrs: {
              version = "0.8.220";
              src = pkgs.fetchFromGitHub {
                owner = "zevlg";
                repo = "telega.el";
                rev = "c522d366aebcdf2178c34978f7f5d3167840a641";
                sha256 = "0vsdcdj3b9sbmjcihd4zqfdnspdjh2806c77kzsl62jpyj55zkj4";
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
        tdlib # Cross-platform library for building Telegram clients
      ];

      # Install emacs icons symbols if we have any kind of graphical emacs
      fonts.packages = with pkgs; [emacs-all-the-icons-fonts];
    }
    (mkIf cfg.mails.enable {
      # environment.systemPackages = with pkgs; [afew isync mu notmuch notmuch.emacs];
      environment.systemPackages = with pkgs; [
        notmuch.emacs
      ];
    })

    (mkIf cfg.enableSpelling {
      environment.systemPackages = with pkgs; [hunspell hunspellDicts.it_IT hunspellDicts.en_US];
    })
  ]);
}
