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
              # version = "1.8.160";
              version = "1.8.200";
              src = pkgs.fetchFromGitHub {
                owner = "zevlg";
                repo = "telega.el";
                # rev = "17bfa50c8f2e70daeb8866f3bf195f15623ab520";
                # sha256 = "1057zr4g8llxmzy47l5klyi89x66q8qx5vrd50pmpsp4c6772jz9";
								rev = "93fedcefd6585dd98d649e2d8b8ca393e288f2fd";
								sha256 = "0d88mc3as1q1lkzjxnbiq8an80nyd9xkz2d9gbcfdfpd3ggm03mz";
              };
            }))
            vterm
            pdf-tools
            treesit-grammars.with-all-grammars
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
      environment.systemPackages = with pkgs; [afew isync libsecret mu notmuch notmuch.emacs thunderbirdPackages.thunderbird-115]; # thunderbird
      programs.msmtp = {
        enable = true;
      };
    })

    (mkIf cfg.enableSpelling {
      environment.systemPackages = with pkgs; [hunspell hunspellDicts.it_IT hunspellDicts.en_US];
    })
  ]);
}
