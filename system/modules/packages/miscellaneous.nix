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
      
      poppler                              # A PDF rendering library
      xdg-user-dirs                        # Find an XDG user dir
    ];
    
  };
  
}
