{ config, lib, ... }:
with lib;
let
  cfg = config.valentino.modules.term.kitty;
  inherit (config.valentino.modules) themes;
  inherit (config.valentino.modules.shell) bash fish zsh;
in
{
  options.valentino.modules.term.kitty.enable = mkEnableOption "kitty configuration";

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font =
        let
          inherit (themes.font.term) family size;
        in
        {
          inherit size;
          name = family;
        };
      themeFile = if themes.darkTheme then "Modus_Vivendi" else "Modus_Operandi";
      settings = {
        shell = if fish.enable then "fish" else "bash";
        scrollback_lines = 10000;
        show_hyperlink_targets = "yes";
        enable_audio_bell = false;
      };
      shellIntegration = {
        enableBashIntegration = bash.enable;
        enableFishIntegration = fish.enable;
        enableZshIntegration = zsh.enable;
      };
    };

  };
}
