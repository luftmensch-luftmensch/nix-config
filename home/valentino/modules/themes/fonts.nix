{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.themes.font;

  mkFontOption = defaultFamily: {
    family = mkOption {
      type = types.str;
      default = defaultFamily;
    };

    package = mkOption {
      type = types.package;
      default = null;
    };

    size = mkOption {
      type = types.int;
      default = 12;
    };
  };
in
{
  options.valentino.modules.themes.font = {
    enable = lib.mkEnableOption "whether to enable font profiles";
    regular = mkFontOption "Sans";
    monospace = mkFontOption "Monospace";
    term = mkFontOption "Monospace";
    bar = mkFontOption "Monospace";
  };

  config = {
    fonts.fontconfig.enable = true;
    home = {
      packages = [
        cfg.regular.package
        cfg.monospace.package
        cfg.term.package
        cfg.bar.package
      ];

      file.".config/fontconfig/fonts.conf".text = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
        <fontconfig>
          <match target="pattern">
            <edit name="dpi" mode="assign"><double>140</double></edit>
          </match>
        </fontconfig>
      '';
    };

  };
}
