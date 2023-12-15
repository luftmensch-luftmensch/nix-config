{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.extensions;
  cfgBash = config.valentino.modules.shell.bash;
  cfgZsh = config.valentino.modules.shell.zsh;
  cfgTmux = config.valentino.modules.shell.tmux;
in {
  options.valentino.modules.shell.extensions = {
    enable = mkEnableOption "shell useful commands (e.g. bat, eza) ";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        fd # A much faster lternative to find
      ];
    }

    {
      # Blazing fast grep
      programs.ripgrep = {
        enable = true;
        # arguments = [
        #   "--max-columns=150"
        #   "--max-columns-preview"
        #   "--smart-case"
        # ];
      };
    }

    {
      programs.bat = {
        enable = true;
        config = {
          theme = "base16";
        };
        extraPackages = with pkgs.bat-extras; [batman batdiff batgrep];
      };

      home.shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
      };
    }

    {
      # A better alternative to boring ls
      programs.eza = {
        enable = true;
        # enableAliases = true;
        # git = true;
        # icons = true;
      };
    }

    {
      programs.fzf = {
        enable = true;

        defaultOptions = [
          "--ansi"
          "--reverse"
          "--border"
          "--inline-info"
          "--color=16"
        ];

        enableBashIntegration =
          if cfgBash.enable
          then true
          else false;
        enableZshIntegration =
          if cfgZsh.enable
          then true
          else false;

        tmux.enableShellIntegration =
          if cfgTmux.enable
          then true
          else false;
      };
    }

    {
      programs.nix-index = {
        enable = true;
        enableBashIntegration =
          if cfgBash.enable
          then true
          else false;
        enableZshIntegration =
          if cfgZsh.enable
          then true
          else false;
      };
    }

    {
      programs.htop = {
        enable = true;
        settings = {
          sort_key = 46;
          sort_direction = -1;
          tree_sort_key = 0;
          tree_sort_direction = 1;
          hide_kernel_threads = 1;
          hide_userland_threads = 0;
          # hide_running_in_container=0;
          shadow_other_users = 1;
          shadow_distribution_path_prefix = 0;
          show_thread_names = 0;
          show_program_path = 0;
          highlight_base_name = 1;
          highlight_deleted_exe = 1;
          highlight_megabytes = 1;
          highlight_threads = 1;
          highlight_changes = 0;
          highlight_changes_delay_secs = 5;
          find_comm_in_cmdline = 1;
          strip_exe_from_cmdline = 1;
          show_merged_command = 0;
          tree_view = 1;
          tree_view_always_by_pid = 1;
          all_branches_collapsed = 0;
          header_margin = 1;
          detailed_cpu_time = 1;
          cpu_count_from_one = 0;
          show_cpu_usage = 1;
          show_cpu_frequency = 1;
          show_cpu_temperature = 0;
          degree_fahrenheit = 0;
          update_process_names = 0;
          account_guest_in_cpu_meter = 0;
          color_scheme = 0;
          enable_mouse = 1;
          delay = 15;
          hide_function_bar = 0;
          header_layout = "two_67_33";
          screen_tabs = 1;
          # header_layout=two_50_50;
          column_meters_0 = "LeftCPUs2 CPU Memory Swap NetworkIO DiskIO";
          column_meter_modes_0 = [
            1
            1
            1
            1
            2
            2
          ];
          column_meters_1 = "RightCPUs2 Tasks LoadAverage Uptime Systemd System";
          column_meter_modes_1 = [
            1
            2
            2
            2
            2
            2
          ];

          fields = [
            0
            48
            17
            18
            38
            39
            40
            2
            46
            47
            49
            1
          ];
        };
      };
    }
  ]);
}
