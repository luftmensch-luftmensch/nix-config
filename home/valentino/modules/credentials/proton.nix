{
  config,
  lib,
  pkgs,
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
    pass.enable = mkEnableOption "enable proton-pass";
  };

  config = mkMerge [
    (mkIf cfg.authenticator.enable {
      home.packages = [ unstable-pkgs.proton-authenticator ];
    })
    (mkIf cfg.pass.enable {
      home.packages = [ pkgs.proton-pass ];
    })
  ];
}
