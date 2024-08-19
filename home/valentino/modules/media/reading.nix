{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.media.reading;
in
{
  options.valentino.modules.media.reading = {
    mdx.enable = mkEnableOption "manga support (mdx)";
    izrss.enable = mkEnableOption "feed rss support (izrss)";
  };

  config = mkMerge [
    (mkIf cfg.mdx.enable { home.packages = [ pkgs.mdx-go ]; })
    (mkIf cfg.izrss.enable { home.packages = [ pkgs.izrss ]; })
  ];
}
