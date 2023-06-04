{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: with lib; let 
  cfg = config.system.modules.editor.emacs;
in {
  options.system.modules.editor.emacs = {
    enable          = mkEnableOption "Enable emacs";
    package = mkOption {
      type = types.package;
      default = "default";
      defaultText = "default";
      description = "Which emacs package to use";
    };
    daemon.enable   = mkEnableOption "Enable daemon";
    mails.enable    = mkEnableOption "Enable emacs mails capabilities";
    enableSpelling  = mkEnableOption "Enable emacs spelling capabilities";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];

      # services.emacs = {
      #   enable = true;
      #   package = cfg.package;
      #   package = pkgs.emacs-pgtk;
      # };

      environment.systemPackages = with pkgs; [
        ((emacsPackagesFor cfg.package).emacsWithPackages (epkgs: [ epkgs.melpaPackages.telega epkgs.vterm epkgs.pdf-tools ]))
        exiftool                             # Tool for reading, writing and editing EXIF meta information

        (writeScriptBin "emacs-daemon" ''
          emacs --fg-daemon
        '')
      ];

      # Install emacs icons symbols if we have any kind of graphical emacs
      # fonts.fonts = with pkgs; [ emacs-all-the-icons-fonts ];

      # programs.emacs = {
      #   enable = true;
      #   # package = if (cfg.unstable-branch.enable) then pkgs.emacsPgtk else pkgs.emacs-gtk;
      #   # package = mkIf (cfg.unstable-branch.enable pkgs.emacsPgtk) else pkgs.emacs-gtk;

      #   extraPackages = epkgs: [
      #     epkgs.vterm
      #     epkgs.pdf-tools
      #     epkgs.melpaPackages.telega
      #   ];
      # };
    }
    # (mkIf cfg.daemon.enable {
    #   systemd.user.services.emacs = {
    #     # Needed for Wayland sessions
    #     Unit = {
    #       After = ["default.target"];
    #       PartOf = ["default.target"];
    #     };
    #   };
    # })
    (mkIf cfg.mails.enable {
      environment.systemPackages = with pkgs; [ afew isync libsecret mu notmuch notmuch.emacs thunderbird ];
      programs.msmtp = {
        enable = true;
      };
    })

    (mkIf cfg.enableSpelling {
      environment.systemPackages = with pkgs; [ hunspell hunspellDicts.it_IT hunspellDicts.en_US ];
    })
  ]);
}
