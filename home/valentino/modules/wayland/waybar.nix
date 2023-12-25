{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.wayland.waybar;
  cfgTheme = config.valentino.modules.themes;
  inherit (config.colorScheme) colors;

  # Function to simplify making waybar outputs
  # https://github.com/Misterio77/nix-config/blob/main/home/misterio/features/desktop/common/wayland-wm/waybar.nix
  jsonOutput = name: {
    pre ? "",
    text ? "",
    tooltip ? "",
    alt ? "",
    class ? "",
    percentage ? "",
  }: "${pkgs.writeShellScriptBin "waybar-${name}" ''
    set -euo pipefail
    ${pre}
    ${pkgs.jq}/bin/jq -cn \
      --arg text "${text}" \
      --arg tooltip "${tooltip}" \
      --arg alt "${alt}" \
      --arg class "${class}" \
      --arg percentage "${percentage}" \
      '{text:$text,tooltip:$tooltip,alt:$alt,class:$class,percentage:$percentage}'
  ''}/bin/waybar-${name}";
in {
  options.valentino.modules.wayland.waybar = {
    enable = mkEnableOption "waybar configuration";

    default_output = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    output_alt = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    battery = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    temperature = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    programs.waybar = {
      enable = true;
      package = pkgs.waybar.override {pulseSupport = true;};
			settings = {
				primary = {
					layer = "top";
					height = "auto";
					position = "bottom";
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
					modules-right =
						[
						"idle_inhibitor"
							"pulseaudio"
							"network"
						]
						++ (optionals (cfg.battery != null) [
								"battery"
						])
						++ [
						"cpu"
							"memory"
							"tray"
						];
				};

				secondary = {
					layer = "top";
					height = "auto";
					position = "bottom";
					output = optionalAttrs (cfg.output_alt != null) "${cfg.output_alt}";

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
							"custom/mail"
							"temperature"
					];
				};

				modules = {
					"sway/window" = {
						max-length = 50;
						on-click = "sleep 0.1 ; pgrep ${pkgs.slurp}/bin/slurp || (${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -r '.. | select(.pid? and .visible?) | .rect | \"\\(.x);\\(.y) \\(.width)x\\(.height)\"' | XCURSOR_SIZE=48 ${pkgs.slurp}/bin/slurp -w 1 -c A5BAD1 -s C3DFFE94 | ${pkgs.grim}/bin/grim -g - ~/$(date +'Screenshot_%Y-%m-%d_%H%M%S.png'))";
						on-click-right = "sleep 0.1 ; pgrep ${pkgs.slurp}/bin/slurp || (XCURSOR_SIZE=48 ${pkgs.slurp}/bin/slurp -w 1 -c A5BAD1 -s C3DFFE94 | ${pkgs.grim}/bin/grim -g - ~/$(date +'Screenshot_%Y-%m-%d_%H%M%S.png'))";
						on-click-middle = "${pkgs/sway}/bin/swaymsg kill";
					};

					"sway/workspaces" = {
						disable-scroll = true;
						all-outputs = false;
						format = "   {name}   ";
					};

					"wlr/taskbar" = {
						format = "{icon}";
						tooltip-format = "{title}";
						icon-theme = "Adwaita";
						icon-size = 22;
						on-click = "activate";
						on-click-middle = "close";
					};

					"sway/mode" = {
						format = "<span style=\"italic\"> {}</span>";
					};

					idle_inhibitor = {
						format = "{icon}";
						format-icons = {
							activated = "";
							deactivated = "";
						};
					};

					tray = {
						spacing = 10;
					};

# FIXME: Hardcoded config
					clock = {
						timezone = "Europe/Rome";
						format = " {:%H:%M}";
						format-alt = " {:%A; %d %B, %Y}";
						tooltip-format = "<span size='12pt'>{calendar}</span>";
						tooltip = true;
						calendar = {
							mode = "month";
							mode-mon-col = 3;
							weeks-pos = "";
							on-scroll = 1;
							on-click-middle = "mode";
							format = {
								months = "<span color='#ffead3'><b>{}</b></span>";
								days = "<span color='#ecc6d9'><b>{}</b></span>";
								weeks = "<span color='#99ffdd'><b>W{}</b></span>";
								weekdays = "<span color='#ffcc66'><b>{}</b></span>";
								today = "<span color='#ff6699'><b><u>{}</u></b></span>";
							};
						};
						actions = {
							on-click-middle = "mode";
							on-click-forward = "tz_up";
							on-click-backward = "tz_down";
						};
					};

					cpu = {
						format = "  {avg_frequency}Gz";
						on-click = "${pkgs.foot}/bin/foot -e htop";
					};

					memory = {
						format = "  {used:0.1f} G";
						on-click = "${pkgs.foot}/bin/foot -e htop";
					};

					temperature = {
						hwmon-path = "${cfg.temperature}";
						format = "{temperatureC}°C {icon}";
# format-critical = "{temperatureC}°C";
						critical-threshold = 80;
						interval = 10;
					};

					battery = optionalAttrs (cfg.battery != null) {
						states = {
							warning = 30;
							critical = 15;
						};

						bat = "${cfg.battery}";
						interval = 30;
						format = "{capacity} % {icon}";
						format-charging = " {capacity} %";
						format-plugged = "{capacity} %  ";
						format-alt = "{time} {icon}";
						format-full = "{capacity} % ";
						format-icons = ["" "" "" "" ""]; #  
							tooltip-format = "{time} ({power} )";
					};

					network = {
# "interface"= "wlp2*"; // (Optional) To force the use of this interface
# "format-wifi"= "  {essid}%";
						format-wifi = "  {signalStrength}%";
						format-ethernet = "{ifname}: {ipaddr}/{cidr} ";
						format-linked = "{ifname} (No IP) ";
						format-disconnected = "Disconnected ⚠";
						format-alt = "{ifname}: {ipaddr}/{cidr}";

						tooltip-format = "{ifname} via {gwaddr}";
						tooltip-format-wifi = "{essid}  | Signal: {signalStrength}% | Download: {bandwidthDownBits}; Upload: {bandwidthUpBits}";
						tooltip-format-ethernet = "{ifname} {ipaddr} ";
						tooltip-format-disconnected = "Disconnected";
						on-click-right = "${pkgs.networkmanagerapplet}/bin/nm-connection-editor";
						tooltip = true;
					};

					pulseaudio = {
# scroll-step"= 1; // %, can be a float
						format = "{icon} {volume}% {format_source}";
						format-bluetooth = "{icon} {volume}%  {format_source}";
						format-bluetooth-muted = "    {format_source}";
						format-muted = " {format_source}";
						format-source = "{volume}% ";
						format-source-muted = "";
						format-icons = {
							headphone = " ";
# hands-free"= " ";
# headset"= " ";
# phone"= "";
# portable"= "";
# car"= "";
							default = [" " " " " "];
						};
						on-click = "pavucontrol";
					};

					"custom/weather" = {
						format = "{}     ";
						exec = "curl -H 'Accept-Language: it' 'https://wttr.in/Naples?format=%c+%C++%t'";
						interval = 900;
					};
				};
			};

      style = ''
        * {
            border: none;
            border-radius: 0;
            font-family: "${cfgTheme.font.bar.family}";
            font-size: ${toString cfgTheme.font.bar.size}px;
            font-weight: normal;
        }

        #custom-menu {
           background-color: #${colors.base0C};
            color: #${colors.base00};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 22px;
            border-radius: 14px;
        }

        #custom-hostname {
            background-color: #${colors.base0C};
            color: #${colors.base00};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 5px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 14px;
            border-radius: 14px;
        }

        tooltip {
            background-color: #${colors.base00};
            color: #${colors.base07};
            border-radius: 10px;
            padding: 4px;
        }

        window#waybar {
            background-color: #${colors.base00};
            color: #${colors.base07};
            transition-property: background-color;
            transition-duration: 0.5s;
        }

        #idle_inhibitor {
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            padding-left: 12px;
            padding-right: 12px;
            border-radius: 14px;
        }

        #workspaces {
            margin-left: 4px;
            margin-right: 4px;
        }

        #workspaces button {
            background: #${colors.base02};
            color: #${colors.base07};
            padding: 0 8px;
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 2px;
            margin-right: 2px;
            border-radius: 14px;
            transition-duration: 0.2s;
        }

        #workspaces button:hover {
            background-color: #${colors.base03};
            color: #${colors.base07};
            transition-duration: 0.2s;
        }

        #workspaces button.active {
            background-color: #${colors.base0B};
            color: #${colors.base00};
            padding: 0 12px;
            transition-duration: 0.2s;
        }

        #workspaces button.focused {
            background-color: #${colors.base0B};
            color: #${colors.base00};
            padding: 0 12px;
            transition-duration: 0.2s;
        }

        #workspaces button.urgent {
            background: #${colors.base08};
            color: #${colors.base00};
        }

        #tray,
        #cpu,
        #temperature,
        #memory,
        #battery,
        #pulseaudio,
        #clock {
            padding: 0 10px;
            color: #${colors.base07};
            margin-top: 5px;
            margin-bottom: 5px;
            margin-left: 5px;
            margin-right: 5px;
            border-radius: 14px;
        }

        /* Tray */
        #tray {
            background-color: #${colors.base02};
            color: #${colors.base00};
        }

        #tray > .passive {
            -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
            -gtk-icon-effect: highlight;
            background-color: #eb4d4b;
        }

        /* CPU */
        #cpu {
            background-color: #${colors.base0C};
            color: #${colors.base00};
        }

        /* Temperature */
        #temperature {
            background-color: #${colors.base09};
            color: #${colors.base00};
        }

        /* Memory */
        #memory {
            background-color: #${colors.base0B};
            color: #${colors.base00};
        }

        /* Battery */
        #battery {
            background-color: #${colors.base0A};
            color: #${colors.base00};
        }

        /* Audio */
        #pulseaudio {
            background-color: #${colors.base0E};
            color: #${colors.base00};
        }

        /* Clock */
        #clock {
            background-color: #${colors.base0D};
            color: #${colors.base00};
        }
      '';
    };
  };
}
