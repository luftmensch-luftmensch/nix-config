{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.xorg.clipcat;
  socketPath = "${config.xdg.configHome}/clipcat/grpc.sock";
in
{
  options.valentino.modules.xorg.clipcat.enable =
    mkEnableOption "Xorg clipboard configuration using clipcat";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.clipcat ];
    services.clipcat = {
      enable = true;

      daemonSettings = {
        daemonize = true;
        pid_file = "${config.xdg.configHome}/clipcat/clipcatd.pid";
        primary_threshold_ms = 5000;
        max_history = 50;
        synchronize_selection_with_clipboard = true;
        history_file_path = "${config.xdg.configHome}/clipcat/clipcatd-history";
        snippets = [ ];
        log = { };
        watcher = {
          enable_clipboard = true;
          enable_primary = true;
          enable_secondary = false;
          sensitive_mime_types = [ "x-kde-passwordManagerHint" ];
          filter_text_min_length = 1;
          filter_text_max_length = 20000000;
          denied_text_regex_patterns = [ ];
          capture_image = true;
          filter_image_max_size = 5242880;
        };
        grpc = {
          enable_http = true;
          enable_local_socket = true;
          host = "127.0.0.1";
          port = 45045;
          local_socket = socketPath;
        };
        dbus.enable = true;
        metrics = {
          enable = true;
          host = "127.0.0.1";
          port = 45047;
        };
        desktop_notification.enable = false;
      };

      ctlSettings.server_endpoint = socketPath;
    };
  };
}
