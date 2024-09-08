{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.system.modules.hardware.video;
in
{
  options.system.modules.hardware.video = {
    i2c.enable = mkEnableOption "Enable i2c capabilities";
  };

  config = (mkMerge [
    (mkIf cfg.i2c.enable { hardware.i2c.enable = true; })
  ]);
}
