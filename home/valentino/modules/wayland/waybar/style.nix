{
  theme,
  palette,
  ...
}: {
  style = ''
    * {
      border: none;
      border-radius: 0;
      font-family: "${theme.font.bar.family}";
      font-size: ${toString theme.font.bar.size}px;
      font-weight: normal;
    }

    #custom-menu {
      background-color: #${palette.base0C};
      color: #${palette.base00};
      margin-top: 5px;
      margin-bottom: 5px;
      margin-left: 2px;
      margin-right: 2px;
      padding-left: 12px;
      padding-right: 22px;
      border-radius: 14px;
    }

    #custom-hostname {
      background-color: #${palette.base0C};
      color: #${palette.base00};
      margin-top: 5px;
      margin-bottom: 5px;
      margin-left: 5px;
      margin-right: 2px;
      padding-left: 12px;
      padding-right: 14px;
      border-radius: 14px;
    }

    tooltip {
      background-color: #${palette.base00};
      color: #${palette.base07};
      border-radius: 10px;
      padding: 4px;
    }

    window#waybar {
      background-color: #${palette.base00};
      color: #${palette.base07};
      transition-property: background-color;
      transition-duration: 0.5s;
    }

    window#waybar.hidden {
      opacity: 0.2;
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

    #idle_inhibitor.activated {
      background-color: #${palette.base03};
      color: #${palette.base00};
    }

    #idle_inhibitor.deactivated {
      background-color: #${palette.base02};
      color: #${palette.base00};
    }

    #workspaces button {
      background: #${palette.base01};
      color: #${palette.base07};
      margin: 0.2rem;
      border-radius: 0.2em;
      transition-duration: 0.2s;
    }

    #workspaces button:hover {
      background-color: #${palette.base0D};
      color: #${palette.base07};
      transition-duration: 0.2s;
    }

    #workspaces button.active {
      background-color: #${palette.base0B};
      color: #${palette.base00};
      padding: 0 6px;
      transition-duration: 0.2s;
    }

    #workspaces button.focused {
      background-color: #${palette.base05};
      color: #${palette.base00};
      padding: 0 6px;
      transition-duration: 0.2s;
    }

    #workspaces button.urgent {
      background: #${palette.base08};
      color: #${palette.base00};
    }

    #tray,
    #cpu,
    #temperature,
    #memory,
    #battery,
    #pulseaudio,
    #network,
    #clock,
    #idle_inhibitor {
      padding: 0 10px;
      color: #${palette.base07};
      margin: 0.2rem;
      border-radius: 14px;
    }

    #network {
      background-color: #${palette.base02};
    }

    #tray {
      background-color: #${palette.base02};
      color: #${palette.base00};
    }

    #tray > .passive {
      -gtk-icon-effect: dim;
    }

    #tray > .needs-attention {
      -gtk-icon-effect: highlight;
      background-color: #eb4d4b;
    }

    #cpu {
      background-color: #${palette.base0C};
      color: #${palette.base00};
    }

    #temperature {
      background-color: #${palette.base09};
      color: #${palette.base00};
    }

    #memory {
      background-color: #${palette.base0B};
      color: #${palette.base00};
    }

    #battery {
      background-color: #${palette.base0A};
      color: #${palette.base00};
    }

    #battery.warning {
      background-color: #${palette.base09};
      color: #${palette.base00};
    }

    @keyframes blink {
      to {
        background-color: #${palette.base06};
        color: #${palette.base00};
      }
    }

    #battery.critical:not(.charging) {
      background-color: #${palette.base09};
      color: #${palette.base06};
      animation-name: blink;
      animation-duration: 0.5s;
      animation-timing-function: linear;
      animation-iteration-count: infinite;
      animation-direction: alternate;
    }

    #pulseaudio {
      background-color: #${palette.base0E};
      color: #${palette.base00};
    }

    #clock {
      background-color: #${palette.base0D};
      color: #${palette.base00};
    }
  '';
}
