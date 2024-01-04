{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.manpages;
in {
  options.system.modules.dev.manpages = {
    enable = mkEnableOption "dev manpages";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [man-pages man-pages-posix];
    documentation = {
      # Remove bloatware (NixOS HTML file)
      nixos.enable = false;
      man = {
        enable = true;
        # This allows searching for a page or keyword using utilities
        # like apropos(1) and the -k option of man(1)
        generateCaches = true;
      };

      dev.enable = true;
      doc.enable = true;
      info.enable = true;
    };
  };
}
