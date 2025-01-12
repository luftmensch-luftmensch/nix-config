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
        background_opacity = 0.9;
        background_blur = 1;
        cursor_shape = "beam";
        cursor_blink_interval = 0;
        clear_all_shortcuts = true;
        detect_urls = true;
        url_style = "double";
        url_prefixes = "http https file ftp gemini irc gopher mailto news git";
        remember_window_size = false;
        allow_remote_control = true;
        confirm_os_window_close = 0;
        close_on_child_death = true;
        scrollback_pager = "${config.home.sessionVariables.PAGER}";
      };

      keybindings = {
        "ctrl+shift+c" = "copy_to_clipboard";
        "ctrl+v" = "paste_from_clipboard";
        "ctrl+shift+v" = "paste_from_clipboard";
        "ctrl+shift+t" = "launch --cwd=current --type=os-window";
        "ctrl+shift+n" = "launch --cwd=current --type=os-window";
        "ctrl+shift+e" = "kitten hints --type url --hints-text-color red";
        "ctrl+shift+g" = "show_last_command_output";
      };
      shellIntegration = {
        enableBashIntegration = bash.enable;
        enableFishIntegration = fish.enable;
        enableZshIntegration = zsh.enable;
      };
    };

  };
}
