{pkgs, ...}: let
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
  # Shared modules
  "custom/menu" = {
    return-type = "json";
    exec = jsonOutput "menu" {
      text = "";
      tooltip = ''$(cat /etc/os-release | grep PRETTY_NAME | cut -d '"' -f2)'';
    };
    on-click = "rofi-powermenu";
  };
  "sway/workspaces" = {
    disable-scroll = true;
    all-outputs = false;
    format = "{name}";
  };

  "sway/mode" = {
    format = "<span style=\"italic\"> {}</span>";
  };

  "sway/window" = let
    _grim = "${pkgs.grim}/bin/grim -g";
    _slurp = "${pkgs.slurp}/bin/slurp";
    _swaymsg = "${pkgs.sway}/bin/swaymsg";

    # I fucking hate this mess but I cannot find a way to escape this
    _screenshot_current_window = pkgs.writeShellScriptBin "_screenshot_current_window" ''
      ${_grim} "$(${_swaymsg} -t get_tree | ${pkgs.jq}/bin/jq -j '.. | select(.type?) | select(.focused).rect | "\(.x),\(.y) \(.width)x\(.height)"')" ~/$(date +'Screenshot_%Y-%m-%d_%H%M%S.png')
    '';

    _cmd_r = "(XCURSOR_SIZE=48 ${_slurp} -w 1 -c A5BAD1 -s C3DFFE94 | ${_grim} -g - ~/$(date +'Screenshot_%Y-%m-%d_%H%M%S.png'))";
  in {
    max-length = 50;
    on-click = "${_screenshot_current_window}/bin/_screenshot_current_window";
    on-click-right = "sleep 0.1 ; pgrep ${_slurp} || ${_cmd_r}";
    on-click-middle = "${_swaymsg} kill";
  };

  clock = {
    timezone = "Europe/Rome";
    format = " {:%H:%M}";
    format-alt = " {:%A; %d %B, %Y}";
    # Fonts bigger than this does not display the full year calendar (Maybe for spacing reason?)
    # tooltip-format = "<small>\n<span size='10pt'>{calendar}</span></small>";
    # tooltip-format = "<small>{calendar}</small>";
    tooltip-format = "{calendar}";
    calendar = {
      mode = "month";
      # Count of months per row (Relevant for mode=year)
      mode-mon-col = 3;
      weeks-pos = "";
      on-scroll = 1;
      on-click-right = "mode";
      format = {
        months = "<span color='#ffead3'><b>{}</b></span>";
        days = "<span color='#ecc6d9'><b>{}</b></span>";
        weeks = "<span color='#99ffdd'><b>W{}</b></span>";
        weekday = "<span color='#ffcc66'><b>{}</b></span>";
        today = "<span color='#ff6699'><b><u>{}</u></b></span>";
      };
    };
    actions = {
      on-click-middle = "mode";
      on-click-forward = "tz_up";
      on-click-backward = "tz_down";
      # on-scroll-up = "shift_up";
      # on-scroll-down = "shift_down";
    };
  };

  # Default monitor specific modules
  idle_inhibitor = {
    format = "{icon}";
    format-icons = {
      activated = " "; # " ";
      deactivated = " "; #  " ";
    };
  };

  pulseaudio = {
    format = "{icon} {volume}% {format_source}";
    format-bluetooth = "() {icon} {volume}% {format_source}";
    format-bluetooth-muted = "()   {format_source}";
    format-muted = " {format_source}";
    format-source = "{volume}% ";
    format-source-muted = "";
    format-icons = {
      headphone = " 󰋋 ";
      hands-free = " ";
      headset = "󰋎 ";
      phone = "";
      # portable= "";
      car = "";
      default = [" " " " " "];
    };
    on-click = "pavucontrol";
  };

  network = {
    format-wifi = "  {signalStrength}%";
    format-ethernet = "{ifname}: {ipaddr}/{cidr} ";
    format-linked = "{ifname} (No IP) ";
    format-disconnected = "Disconnected ⚠";
    format-alt = "{ifname}: {ipaddr}/{cidr}";

    tooltip-format = "{ifname} via {gwaddr}";
    tooltip-format-wifi = "{essid}  | Signal: {signalStrength}% | 󰇚 {bandwidthDownBits}; 󰕒 {bandwidthUpBits}";
    tooltip-format-ethernet = "{ifname} {ipaddr} 󰈀";
    tooltip-format-disconnected = "Disconnected";
    on-click-right = "${pkgs.networkmanagerapplet}/bin/nm-connection-editor";
    tooltip = true;
  };

  cpu = {
    format = "  {avg_frequency}Gz";
    on-click = "${pkgs.foot}/bin/foot -e btop";
  };

  memory = {
    format = "  {used:0.1f} G";
    on-click = "${pkgs.foot}/bin/foot -e btop";
  };

  tray.spacing = 10;

  "wlr/taskbar" = {
    format = "{icon}";
    tooltip-format = "{title}";
    icon-theme = "Adwaita";
    icon-size = 22;
    on-click = "activate";
    on-click-middle = "close";
  };

  backlight = {
    interval = 30;
    align = 0;
    rotate = 0;
    #"device": "amdgpu_bl0",
    format = "{icon} {percent}%";
    format-icons = [
      "󰃞 "
      "󰃝 "
      "󰃟 "
      "󰃠 "
    ];
    on-click = "";
    on-click-middle = "";
    on-click-right = "";
    on-update = "";
    on-scroll-up = "brightnessctl s 5%+";
    on-scroll-down = "brightnessctl s 5%-";
    smooth-scrolling-threshold = 1;
  };

  battery = {
    states = {
      warning = 30;
      critical = 15;
    };
    interval = 50;
    format = "{capacity} % {icon}";
    format-charging = " {capacity} %";
    format-plugged = "{capacity} %  ";
    format-alt = "{time} {icon}";
    format-full = "{capacity} % ";
    format-icons = ["" "" "" "" ""];
    tooltip-format = "{time} ({power} )";
    tooltip-format-charging = "{timeTo} ({power} )";
  };

  # External monitor specific modules
  "custom/weather" = {
    format = "{}     ";
    exec = "${pkgs.curl}/bin/curl -H 'Accept-Language: it' 'https://wttr.in/Naples?format=%c+%C++%t'";
    interval = 900;
  };

  temperature = {
    format = "{temperatureC}°C {icon}";
    # format-critical = "{temperatureC}°C";
    critical-threshold = 80;
    interval = 10;
  };
}
