{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.miscellaneous;
in {
  options.system.modules.packages.miscellaneous = {
    enable = mkEnableOption "Enable uncategorized packages";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libreoffice                          # Variant of openoffice.org
      libnotify                            # A library that sends desktop notifications to a notification daemon (Gonna hel dunst!)
      openssl                              # Cryptography toolkit implementing TLS network protocol
      pandoc                               # General markup converter
      playerctl                            # Mpris media player command-line controller
      
      poppler                              # A PDF rendering library
      texlive.combined.scheme-full         # TeX Live environment for scheme-full
      tectonic                             # self-contained TEX/LaTex engine (Guide at https://tectonic-typesetting.github.io/book/latest/index.html)
      xdg-user-dirs                        # Find an XDG user dir
    ];
    
  };
  
}
