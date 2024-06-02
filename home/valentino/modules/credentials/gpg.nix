{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.credentials.gpg;
in
{
  options.valentino.modules.credentials.gpg.enable = mkEnableOption "gpg-agent user configuration";

  config = mkIf cfg.enable {
    programs.gpg.enable = true;

    services.gpg-agent =
      {
        enable = true;
        enableSshSupport = true;
      }
      // lib.optionalAttrs (builtins.hasAttr "pinentryFlavor" config.services.gpg-agent) {
        pinentryPackage = "gnome3";
      }
      // lib.optionalAttrs (builtins.hasAttr "pinentryPackage" config.services.gpg-agent) {
        pinentryPackage = pkgs.pinentry-gnome3;
      };
  };
}
