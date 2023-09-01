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
        ((emacsPackagesFor cfg.package).emacsWithPackages (epkgs: [epkgs.melpaPackages.telega epkgs.vterm epkgs.pdf-tools]))
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
