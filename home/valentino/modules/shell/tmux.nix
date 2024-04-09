{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.tmux;
  theme = with config.colorScheme.palette; ''
    thm_bg=\"#${strings.toLower base00}\"
    thm_fg=\"#${strings.toLower base05}\"
    thm_cyan=\"#${strings.toLower base0C}\"
    thm_black=\"#${strings.toLower base01}\"
    thm_gray=\"#${strings.toLower base01}\"
    thm_magenta=\"#${strings.toLower base0E}\"
    thm_pink=\"#${strings.toLower base0E}\"
    thm_red=\"#${strings.toLower base08}\"
    thm_green=\"#${strings.toLower base0B}\"
    thm_yellow=\"#${strings.toLower base09}\"
    thm_blue=\"#${strings.toLower base0D}\"
    thm_orange=\"#${strings.toLower base0A}\"
    thm_black4=\"#${strings.toLower base00}\"
  '';
in {
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
      # terminal = "\${TERM}";
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
          plugin = tmuxPlugins.catppuccin.overrideAttrs (_: {
            version = "unstable-2024-03-30";
            src = pkgs.fetchFromGitHub {
              owner = "catppuccin";
              repo = "tmux";
              rev = "5ed4e8a6a20c928688da268dfcdf460ac9c3cb49";
              sha256 = "sha256-k9Ihfk8C1jYkGUvOcgLwS4UdXR8d/4Nu/Dyh03FpDZc=";
            };
            postInstall = ''
              echo "${theme}" > $target/catppuccin-dynamic.tmuxtheme
            '';
          });
          extraConfig = with config.colorScheme.palette; ''
            set -g @catppuccin_flavour "dynamic"

            set -g @catppuccin_status_background "default"

            set -g @catppuccin_status_left_separator "█"
            set -g @catppuccin_status_right_separator "█"
            set -g @catppuccin_window_middle_separator " █"
            set -g @catppuccin_window_number_position "right"

            set -g @catppuccin_window_default_fill "number"
            set -g @catppuccin_window_default_text "#W"

            set -g @catppuccin_window_current_fill "number"
            set -g @catppuccin_window_current_text "#W"
            set -g @catppuccin_window_current_background "#${strings.toLower base02}"

            set -g @catppuccin_status_modules_right "directory session"

            set -g @catppuccin_weather_icon " "
            set -g @catppuccin_uptime_icon "󰔟 "
            set -g @catppuccin_load_icon "󰊚 "
            set -g @catppuccin_battery_icon "#{battery_icon}"
            set -g @catppuccin_host_icon "󰒋 "
            set -g @catppuccin_cpu_icon " "
            set -g @catppuccin_application_icon " "
            set -g @catppuccin_clima_icon " "
            set -g @catppuccin_directory_icon " "
            set -g @catppuccin_date_time_icon "󰃰 "
            set -g @catppuccin_user_icon " "
            set -g @catppuccin_session_icon " "
          '';
        }
      ];
    };
  };
}
