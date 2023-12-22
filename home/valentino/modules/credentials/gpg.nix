{
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.credentials.gpg;
in {
  options.valentino.modules.credentials.gpg = {
    enable = mkEnableOption "gpg-agent user configuration";
  };

  config = mkIf cfg.enable {
    programs.gpg = {
      enable = true;
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gnome3"; # Other possible values: "curses", "tty", "gtk2", "qt", "emacs"
    };
  };
}
