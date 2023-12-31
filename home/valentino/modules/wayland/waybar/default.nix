{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.wayland.waybar;
  theme = config.valentino.modules.themes;
  inherit (config.colorScheme) colors;
in {
  options.valentino.modules.wayland.waybar = {
    enable = mkEnableOption "waybar configuration";

    default_output = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    external_output = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    programs.waybar = let
      style = import ./style.nix {
        inherit theme colors;
      };

      custom_modules = import ./modules.nix {
        inherit pkgs;
      };
    in {
      enable = true;
      package = pkgs.waybar.override {pulseSupport = true;};
      inherit (style) style;
      settings = [
        # Default monitor
        {
          layer = "top";
          position = "bottom";
          # height = "auto";
          output = optionalAttrs (cfg.default_output != null) "${cfg.default_output}";

          modules-left =
            [
              "custom/menu"
            ]
            ++ (optionals config.wayland.windowManager.sway.enable [
              "sway/workspaces"
              "sway/mode"
              "sway/window"
            ])
            ++ (optionals config.wayland.windowManager.hyprland.enable [
              "hyprland/workspaces"
            ]);

          modules-center = ["clock"];
          modules-right = [
            "idle_inhibitor"
            "pulseaudio"
            "network"
            "battery"
            "cpu"
            "memory"
            "tray"
          ];
          # Shared modules
          inherit (custom_modules) "custom/menu" "sway/workspaces" "sway/mode" "sway/window" clock;
          # Specific modules
          inherit (custom_modules) "idle_inhibitor" "pulseaudio" "network" "battery" "cpu" "memory" "tray";
        }

        # External monitor
        {
          layer = "top";
          output = optionalAttrs (cfg.external_output != null) "${cfg.external_output}";
          position = "bottom";
          # height = "auto";

          modules-left =
            [
              "custom/menu"
            ]
            ++ (optionals config.wayland.windowManager.sway.enable [
              "sway/workspaces"
              "sway/mode"
              "sway/window"
            ])
            ++ (optionals config.wayland.windowManager.hyprland.enable [
              "hyprland/workspaces"
            ]);

          modules-center = ["clock"];
          modules-right = [
            "custom/weather"
            # "custom/mail"
            "temperature"
          ];

          # Shared modules
          inherit (custom_modules) "custom/menu" "sway/workspaces" "sway/mode" "sway/window" clock;
          # Specific modules
          inherit (custom_modules) "custom/weather" "temperature";
        }
      ];
    };
  };
}
