{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.credentials.bitwarden;
in
{
  options.valentino.modules.credentials.bitwarden.enable = mkEnableOption "bitwarden";

  config = mkIf cfg.enable {
    programs.rbw = {
      enable = true;
      settings = {
        email = "valentinobocchetti59@gmail.com";
        pinentry =
          if builtins.typeOf config.programs.rbw == "string" then "gnome3" else pkgs.pinentry-gnome3;
      };
    };

    home.packages = [ pkgs.bitwarden ];
  };
}
