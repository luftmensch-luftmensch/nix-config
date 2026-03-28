{
  lib,
  theme,
  palette,
  pkgs,
  ...
}:
{

  settings = {
    positionX = "right";
    positionY = "top";
    layer = "overlay";
    layer-shell = true;
    cssPriority = "user";
    control-center-layer = "top";
    control-center-margin-top = 2;
    control-center-margin-bottom = 2;
    control-center-margin-right = 1;
    control-center-margin-left = 0;
    notification-window-width = 500;
    control-center-height = 1025;
    notification-icon-size = 64;
    notification-body-image-height = 100;
    notification-body-image-width = 200;
    notification-2fa-action = true;
    timeout = 10;
    timeout-low = 5;
    timeout-critical = 0;
    fit-to-screen = true;
    control-center-width = 500;
    keyboard-shortcuts = true;
    image-visibility = "when-available";
    transition-time = 200;
    hide-on-clear = false;
    hide-on-action = true;
    script-fail-notify = true;

    scripts = { };
    notification-visibility = {
      "example-name" = {
        "state" = "muted";
        "urgency" = "Low";
        "app-name" = "Spotify";
      };
    };
    widgets = [
      "title"
      "dnd"
      "notifications"
      "mpris"
      "volume"
      "backlight"
      "buttons-grid"
    ];
    widget-config = {
      label = {
        max-lines = 1;
        text = "Control Center";
      };
      buttons-grid.actions =
        let
          swaync-client = "${pkgs.swaynotificationcenter}/bin/swaync-client -cp";
          bash = "${lib.getExe pkgs.bash} -c";
        in
        [
          {
            label = "";
            command = "${bash} '${swaync-client} && ${lib.getExe pkgs.kooha}'";
          }
          {
            label = "";
            command = "${bash} '${swaync-client} && ${lib.getExe pkgs.spotify}'";
          }
          {
            label = "";
            command = "${bash} '${swaync-client} && ${lib.getExe pkgs.zathura}'";
          }
          {
            label = "⏻";
            command = "${bash} '${swaync-client} && ${lib.getExe pkgs.rofi-powermenu}'";
          }
        ];
      backlight = {
        label = " ";
        device = "amdgpu_bl1";
        min = 10;
      };
      volume = {
        "label" = "󰕾  ";
        "show-per-app" = true;
      };
      inhibitors = {
        text = "Inhibitors";
        button-text = "Clear All";
        clear-all-button = true;
      };
      title = {
        text = "Notifications";
        clear-all-button = true;
        button-text = "󰆴 Clear All";
      };

      dnd.text = "Do Not Disturb";
      mpris = {
        image-size = 96;
        image-radius = 12;
      };
    };
  };

  style = ''
    @define-color cc-bg #${palette.base00};
    @define-color noti-border-color #${palette.base02};
    @define-color noti-bg #${palette.base01};
    @define-color noti-bg-darker #${palette.base02};
    @define-color noti-bg-hover rgb(40, 40, 40);
    @define-color noti-bg-focus rgba(25, 26, 27, 0.6);
    @define-color text-color #${palette.base06};
    @define-color text-color-disabled #${palette.base02};
    @define-color bg-selected #${palette.base04};

    * {
      font-family: "${theme.font.bar.family}";
      font-weight: normal;
    }

    scrolledwindow > scrollbar {
      opacity: 0;
      min-width: 0px;
    }

    .control-center-list scrollbar,
    .control-center-list scrollbar slider {
      opacity: 0;
      min-width: 0px;
      min-height: 0px;
    }

    .control-center .notification-row:focus,
    .control-center .notification-row:hover {
      opacity: 1;
      background: @noti-bg-darker;
    }

    .notification-row {
      outline: none;
      margin: 20px;
      padding: 0;
    }

    .notification {
      background: transparent;
      margin: 0px;
    }

    .notification-content {
      background: @cc-bg;
      padding: 7px;
      border-radius: 0px;
      border: 2px solid #${palette.base02};  /* #323232 */
      margin: 0;
    }

    .close-button {
      background: #${palette.base0A};        /* yellow-intense #F0DD60 */
      color: @cc-bg;
      text-shadow: none;
      padding: 0;
      border-radius: 0px;
      margin-top: 5px;
      margin-right: 5px;
    }

    .close-button:hover {
      box-shadow: none;
      background: #${palette.base09};        /* orange-intense #FBA849 */
      transition: all 0.15s ease-in-out;
      border: none;
    }

    .notification-action {
      color: #${palette.base05};             /* fg-dim #E0E6F0 */
      border: 2px solid #${palette.base02};
      border-top: none;
      border-radius: 0px;
      background: #${palette.base01};        /* bg-alt #191A1B */
    }

    .notification-default-action:hover,
    .notification-action:hover {
      color: #${palette.base05};
      background: #${palette.base02};        /* bg-active #323232 */
    }

    .summary {
      padding-top: 7px;
      font-size: 13px;
      color: #${palette.base06};             /* fg-main #FFFFFF */
    }

    .time {
      font-size: 11px;
      color: #${palette.base0A};             /* yellow-intense #F0DD60 */
      margin-right: 24px;
    }

    .body {
      font-size: 12px;
      color: #${palette.base05};             /* fg-dim #E0E6F0 */
    }

    .control-center {
      background: @cc-bg;
      border: 2px solid #${palette.base02};
      border-radius: 0px;
    }

    .control-center-list {
      background: transparent;
    }

    .control-center-list-placeholder {
      opacity: 0.5;
    }

    .floating-notifications {
      background: transparent;
    }

    .blank-window {
      background: alpha(black, 0.1);
    }

    .widget-title {
      color: #${palette.base06};
      background: @noti-bg-darker;
      padding: 5px 10px;
      margin: 10px 10px 5px 10px;
      font-size: 1.5rem;
      border-radius: 5px;
    }

    .widget-title > button {
      font-size: 1rem;
      color: @text-color;
      text-shadow: none;
      background: @noti-bg;
      box-shadow: none;
      border-radius: 5px;
    }

    .widget-title > button:hover {
      background: #${palette.base0A};        /* yellow-intense */
      color: @cc-bg;
    }

    .widget-dnd {
      background: @noti-bg-darker;
      padding: 5px 10px;
      margin: 5px 10px 10px 10px;
      border-radius: 5px;
      font-size: large;
      color: #${palette.base05};             /* fg-dim #E0E6F0 */
    }

    .widget-dnd > switch {
      min-width: 6px;
      border-radius: 4px;
      background: #${palette.base02};        /* bg-active #323232 */
    }

    .widget-dnd > switch:checked {
      background: #${palette.base0A};        /* yellow */
      border: 1px solid #${palette.base0A};
    }

    .widget-dnd > switch slider {
      min-width: 20px;
      min-height: 20px;
      background: @cc-bg;
      border-radius: 5px;
    }

    .widget-dnd > switch:checked slider {
      background: @cc-bg;
      border-radius: 5px;
    }

    .widget-label {
      margin: 10px 10px 5px 10px;
    }

    .widget-label > label {
      font-size: 1rem;
      color: @text-color;
    }

    .widget-mpris {
      color: @text-color;
      background: @noti-bg-darker;
      padding: 5px 10px 0px 0px;
      margin: 5px 10px 5px 10px;
      border-radius: 0px;
    }

    .widget-mpris > box > button {
      border-radius: 5px;
    }

    .widget-mpris-player {
      padding: 5px 10px;
      margin: 10px;
    }

    .widget-mpris-title {
      font-weight: 700;
      font-size: 1.25rem;
    }

    .widget-mpris-subtitle {
      font-size: 1.1rem;
    }

    .widget-buttons-grid {
      font-size: x-large;
      padding: 5px;
      margin: 5px 10px 10px 10px;
      border-radius: 5px;
      background: @noti-bg-darker;
    }

    .widget-buttons-grid > flowbox > flowboxchild {
      min-width: 105px;
    }

    .widget-buttons-grid > flowbox > flowboxchild > button {
      margin: 3px;
      background: @cc-bg;
      border-radius: 5px;
      color: @text-color;
    }

    .widget-buttons-grid > flowbox > flowboxchild > button:hover {
      background: #${palette.base0A};        /* yellow-intense */
      color: @cc-bg;
    }

    .widget-menubar > box > .menu-button-bar > button {
      border: none;
      background: transparent;
    }

    .topbar-buttons > button {
      border: none;
      background: transparent;
    }
  '';
}
