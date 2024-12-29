{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.shell.tmux;
in
{
  options.valentino.modules.shell.tmux.enable = mkEnableOption "tmux configuration";

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      prefix = "C-Space";
      newSession = true;
      baseIndex = 1;
      clock24 = true;
      historyLimit = 8000;
      escapeTime = 20;
      terminal = "tmux-256color";
      sensibleOnTop = false;
      mouse = true;

      extraConfig = ''
        set -ga terminal-overrides ",alacritty*:Tc"
        set -g pane-base-index 1 # renumber any window when any is closed
        setw -g automatic-rename on   # rename window to reflect current program
        set -g set-titles on          # set terminal title
        set -g renumber-windows on # renumber any window when any is closed

        set -g mouse on # Enable mouse control (clickable windows, panes, resizable panes)
        setw -g mode-keys vi

        set -g monitor-activity on
        set -g visual-activity off

        bind -n C-l send-keys C-l \; run 'sleep 0.2' \; clear-history # clear both screen and history
        bind C-f command-prompt -p find-session 'switch-client -t %%'
        bind -n M-Left select-pane -L
        bind -n M-Right select-pane -R
        bind -n M-Up select-pane -U
        bind -n M-Down select-pane -D
        bind < swap-pane -D # swap current pane with the next one
        bind > swap-pane -U # swap current pane with the previous one

        # maximize current pane
        bind + run "cut -c3- '#{TMUX_CONF}' | sh -s _maximize_pane '#{session_name}' '#D'"
      '';

      plugins = with pkgs; [
        {
          plugin = tmuxPlugins.catppuccin.overrideAttrs (_: rec {
            version = "2.1.2";
            src = pkgs.fetchFromGitHub {
              owner = "catppuccin";
              repo = "tmux";
              rev = "v${version}";
              sha256 = "sha256-vBYBvZrMGLpMU059a+Z4SEekWdQD0GrDqBQyqfkEHPg=";
            };
          });
          extraConfig = ''
            set -g @catppuccin_flavor "mocha"

            set -g @catppuccin_status_background "none"
            set -g @catppuccin_window_status_style "basic"

            set -g @catppuccin_window_number_position "right"
            set -g @catppuccin_window_current_text "#W"
            set -g @catppuccin_window_current_number_color "#{@thm_green}"

            set -g @catppuccin_status_left_separator "█"
            set -g @catppuccin_status_middle_separator ""
            set -g @catppuccin_status_right_separator "█"

            set -g @catppuccin_directory_text "#{pane_current_path}"

            set -g status-left  ""
            set -g status-right "#{E:@catppuccin_status_directory}#{E:@catppuccin_status_session}"
          '';
        }
      ];
    };
  };
}
