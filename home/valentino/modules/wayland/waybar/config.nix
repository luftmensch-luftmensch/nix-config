{
  theme,
  colors,
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
}
