{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.core.environment;
in {
  options.system.modules.core.environment = {
    enable = mkEnableOption "Enable environment variables";
  };

  config = mkIf cfg.enable {
    environment = {
      # For some reason home-manager fish does not enable tab completion for fish
      pathsToLink = ["/share/fish"];
      variables = {
        QT_QTA_PLATFORMTHEME = "qt5ct";
        QT_QPA_PLATFORMTHEME = "qt5ct";
        #AMD_VULKAN_ICD = "RADV";
        # EDITOR = "emacsclient -t";
        BROWSER = "firefox";
        PAGER = "less --quit-at-eof";
        # GTK_THEME = "Adwaita:dark"; # "Materia-dark";
      };
    };
  };
}
