{schema, ...}: {
  text = ''
    {
      "$schema": "${schema}",
      "positionX": "right",
      "positionY": "top",
      "layer": "overlay",
      "layer-shell": true,
      "cssPriority": "user",
      "control-center-layer": "top",
      "control-center-margin-top": 2,
      "control-center-margin-bottom": 2,
      "control-center-margin-right": 1,
      "control-center-margin-left": 0,
      "notification-icon-size": 64,
      "notification-body-image-height": 100,
      "notification-body-image-width": 90,
      "notification-2fa-action": true,
      "timeout": 10,
      "timeout-low": 5,
      "timeout-critical": 0,
      "fit-to-screen": true,
      "control-center-width": 500,
      "control-center-height": 600,
      "notification-window-width": 500,
      "keyboard-shortcuts": true,
      "image-visibility": "when-available",
      "transition-time": 200,
      "hide-on-clear": false,
      "hide-on-action": true,
      "script-fail-notify": true,
      "scripts": {},
      "notification-visibility": {
        "example-name": {
          "state": "muted",
          "urgency": "Low",
          "app-name": "Spotify"
        }
      },
      "widgets": [
        "label",
        "buttons-grid",
        "backlight",
        "volume",
        "mpris",
        "inhibitors",
        "title",
        "dnd",
        "notifications"
      ],
      "widget-config": {
        "label": {
          "max-lines": 1,
          "text": "Control Center"
        },
        "buttons-grid": {
          "actions": [
            {
              "label": "󰛳",
              "command": "swaync-client -cp && nm-connection-editor"
            },
            {
              "label": "",
              "command": "swaync-client -cp && emacsclient -c"
            },
            {
              "label": "",
              "command": "swaync-client -cp && firefox"
            },
            {
              "label": "",
              "command": "swaync-client -cp && nemo"
            },
            {
              "label": "",
              "command": "swaync-client -cp && zathura"
            }
          ]
        },
        "backlight": {
          "label": " ",
          "device": "amdgpu_bl0",
          "min": 10
        },
        "volume": {
          "label": ""
        },
        "inhibitors": {
          "text": "Inhibitors",
          "button-text": "Clear All",
          "clear-all-button": true
        },
        "title": {
          "text": "Notifications",
          "clear-all-button": true,
          "button-text": "Clear All"
        },
        "dnd": {
          "text": "Do Not Disturb"
        },
        "mpris": {
          "image-size": 96,
          "image-radius": 12
        }
      }
    }
  '';
}
