{
  config,
  lib,
  unstable-pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.credentials.proton;
in
{
  options.valentino.modules.credentials.proton = {
    authenticator.enable = mkEnableOption "enable proton-authenticator";
  };

  config = mkMerge [
    (mkIf cfg.authenticator.enable {
      home.packages = [ unstable-pkgs.proton-authenticator ];
    })
  ];
}
