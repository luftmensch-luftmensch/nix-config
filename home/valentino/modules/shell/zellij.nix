{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.zellij;
  inherit (config.valentino.modules.shell) bash zsh fish;
in {
  options.valentino.modules.shell.zellij = {
    enable = mkEnableOption "enable zellij";
  };

  config = mkIf cfg.enable {
    programs.zellij = {
      enable = true;

      enableBashIntegration = bash.enable;
      enableFishIntegration = fish.enable;
      enableZshIntegration = zsh.enable;

      settings = {
        default_layout = "compact";
        default_mode = "locked";

        on_force_close = "quit";
        pane_frames = true;

        ui.pane_frames = {
          rounded_corners = true;
          hide_session_name = true;
        };

        plugins = {
          tab-bar.path = "tab-bar";
          status-bar.path = "status-bar";
          strider.path = "strider";
          compact-bar.path = "compact-bar";
        };
      };
    };
  };
}
