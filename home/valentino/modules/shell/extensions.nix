{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.shell.extensions;
  inherit (config.valentino.modules.shell) bash zsh tmux;
in
{
  options.valentino.modules.shell.extensions.enable = mkEnableOption "shell useful commands (e.g. bat, eza)";

  config = mkIf cfg.enable {
    home = {
      shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
        rg = "${pkgs.ripgrep}/bin/rg -L";
        htop = "${pkgs.btop}/bin/btop";
      };
      packages = with pkgs; [
        fd
        jq
        wget
      ];

      file.".config/wget/wgetrc".text = ''
        hsts-file=~/.cache/wget-hsts
        # timestamping = on
        # no_parent = on
        # timeout = 60
        # tries = 3
        # retry_connrefused = on
        # trust_server_names = on
        # follow_ftp = on

        # Add a `.html` extension to `text/html` or `application/xhtml+xml` files that lack one, or a `.css` extension to `text/css` files that lack one
        adjust_extension = on

        # Ignore `robots.txt` and `<meta name=robots content=nofollow>`
        robots = off

        # Print the HTTP and FTP server responses
        server_response = on
      '';
    };

    programs = {
      # Blazing fast grep
      ripgrep = {
        enable = true;
        arguments = [
          # "--max-columns=150"
          # "--max-columns-preview"
          # "--smart-case"
        ];
      };

      bat = {
        enable = true;
        config.theme = "base16";
        extraPackages = with pkgs.bat-extras; [
          batman
          batdiff
          batgrep
        ];
      };

      # A better alternative to boring ls
      eza.enable = true;

      fzf = {
        enable = true;
        defaultOptions = [
          "--ansi"
          "--reverse"
          "--border"
          "--inline-info"
          "--color=16"
        ];

        enableBashIntegration = bash.enable;
        enableZshIntegration = zsh.enable;
        tmux.enableShellIntegration = tmux.enable;
      };

      btop = {
        enable = true;
        settings = {
          color_theme = "adwaita";
          theme_background = false;
          vim_keys = true;
          shown_boxes = "cpu mem proc";
          clock_format = "%H:%M";
          presets = "cpu:0:default,mem:0:default,proc:1:default";

          cpu_single_graph = true;
          cpu_bottom = false;
          show_disks = false;
          show_uptime = true;
          # use_fstab = true;

          proc_tree = true;
          proc_colors = true;
          proc_left = true;
          mem_graphs = false;
        };
      };
    };
  };
}
