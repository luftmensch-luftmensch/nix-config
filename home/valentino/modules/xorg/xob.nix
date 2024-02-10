{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg.xob;
  inherit (config.colorScheme) palette;
in {
  options.valentino.modules.xorg.xob = {
    enable = mkEnableOption "A lightweight overlay volume (or anything) bar for the X Window System";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [xob];

    home.file.".config/xob/styles.cfg" = {
      enable = true;
      text = ''
        default = {
            x         = {relative = 0.5; offset = 0;};
            y         = {relative = 0.90; offset = 0;};
            length    = {relative = 0.35; offset = 0;};
            thickness = 24;
            outline   = 3;
            border    = 4;
            padding   = 3;
            orientation = "horizontal";

            overflow = "hidden";

            color = {
                normal = {
                    fg     = "#${palette.base06}";
                    bg     = "#${palette.base00}";
                    border = "#${palette.base06}";
                };
                alt = {
                    fg     = "#555555";
                    bg     = "#00000090";
                    border = "#555555";
                };
                overflow = {
                    fg     = "#${palette.base08}";
                    bg     = "#00000090";
                    border = "#ff0000";
                };
                altoverflow = {
                    fg     = "#550000";
                    bg     = "#00000090";
                    border = "#550000";
                };
            };
        };
      '';
    };
  };
}
