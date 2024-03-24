{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.tmux;
in {
  options.valentino.modules.shell.tmux = {
    enable = mkEnableOption "tmux configuration";
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      prefix = "C-Space";
      newSession = true;
      baseIndex = 1;
      clock24 = true;
      historyLimit = 8000;
      escapeTime = 20;
      terminal = "\${TERM}";
      sensibleOnTop = false;
      mouse = true;

      extraConfig = ''
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
    };
  };
}
