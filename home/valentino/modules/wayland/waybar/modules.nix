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
    format = "   {name}   ";
  };

  "sway/mode" = {
    format = "<span style=\"italic\"> {}</span>";
  };

  "sway/window" = {
    max-length = 50;
    on-click = "sleep 0.1 ; pgrep ${pkgs.slurp}/bin/slurp || (${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -r '.. | select(.pid? and .visible?) | .rect | \"\\(.x);\\(.y) \\(.width)x\\(.height)\"' | XCURSOR_SIZE=48 ${pkgs.slurp}/bin/slurp -w 1 -c A5BAD1 -s C3DFFE94 | ${pkgs.grim}/bin/grim -g - ~/$(date +'Screenshot_%Y-%m-%d_%H%M%S.png'))";
    on-click-right = "sleep 0.1 ; pgrep ${pkgs.slurp}/bin/slurp || (XCURSOR_SIZE=48 ${pkgs.slurp}/bin/slurp -w 1 -c A5BAD1 -s C3DFFE94 | ${pkgs.grim}/bin/grim -g - ~/$(date +'Screenshot_%Y-%m-%d_%H%M%S.png'))";
    on-click-middle = "${pkgs.sway}/bin/swaymsg kill";
  };

  clock = {
    timezone = "Europe/Rome";
    format = " {:%H:%M}";
    format-alt = " {:%A; %d %B, %Y}";
    # Fonts bigger than this does not display the full year calendar (Maybe for spacing reason?)
    tooltip-format = "<span size='10pt'>{calendar}</span>";
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
      activated = " ";
      deactivated = " ";
    };
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

  cpu = {
    format = "  {avg_frequency}Gz";
    on-click = "${pkgs.foot}/bin/foot -e htop";
  };

  memory = {
    format = "  {used:0.1f} G";
    on-click = "${pkgs.foot}/bin/foot -e htop";
  };

  tray = {
    spacing = 10;
  };

  "wlr/taskbar" = {
    format = "{icon}";
    tooltip-format = "{title}";
    icon-theme = "Adwaita";
    icon-size = 22;
    on-click = "activate";
    on-click-middle = "close";
  };

  battery = {
    states = {
      warning = 30;
      critical = 15;
    };

    interval = 30;
    format = "{capacity} % {icon}";
    format-charging = " {capacity} %";
    format-plugged = "{capacity} %  ";
    format-alt = "{time} {icon}";
    format-full = "{capacity} % ";
    format-icons = ["" "" "" "" ""]; #  
    tooltip-format = "{time} ({power} )";
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
