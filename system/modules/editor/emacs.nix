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
              # version = "1.8.200";
              version = "0.8.211";
              src = pkgs.fetchFromGitHub {
                owner = "zevlg";
                repo = "telega.el";
								rev = "aaf5e05f60af786b6aedf469269a422b3a4c1095";
								sha256 = "12jbg6l66f4k8hpv2knczl5v0zicgk125g9dx4jg8g8yi96in6if";
                # rev = "93fedcefd6585dd98d649e2d8b8ca393e288f2fd";
                # sha256 = "0d88mc3as1q1lkzjxnbiq8an80nyd9xkz2d9gbcfdfpd3ggm03mz";
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
      fonts.fonts = with pkgs; [emacs-all-the-icons-fonts];
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
