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
        set -g renumber-windows on # renumber any window when any is closed
        set -g mouse on # Enable mouse control (clickable windows, panes, resizable panes)
        setw -g mode-keys vi

        bind -n M-Left select-pane -L
        bind -n M-Right select-pane -R
        bind -n M-Up select-pane -U
        bind -n M-Down select-pane -D
      '';
    };
  };
}
