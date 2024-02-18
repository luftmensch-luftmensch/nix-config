{
  theme,
  palette,
  ...
}: {
  # TODO: Complete configuration
  text = ''
    @define-color noti-bg rgb(48, 48, 48);
    @define-color noti-bg-hover rgb(56, 56, 56);
    @define-color noti-bg-focus rgba(68, 68, 68, 0.6);
    @define-color noti-close-bg rgba(255, 255, 255, 0.1);
    @define-color noti-close-bg-hover rgba(255, 255, 255, 0.15);

    @define-color bg-selected rgb(0, 128, 255);

    * {
      border: none;
      border-radius: 0;
      font-family: "${theme.font.bar.family}";
      font-size: ${toString theme.font.bar.size}px;
      font-weight: normal;
    }

    .notification-row {
      outline: none;
    }

    .notification-row:focus,
    .notification-row:hover {
      background: @noti-bg-focus;
    }

    .notification {
      border-radius: 12px;
      margin: 6px 12px;
      box-shadow: 0 0 0 1px rgba(0, 0, 0, 0.3), 0 1px 3px 1px rgba(0, 0, 0, 0.7), 0 2px 6px 2px rgba(0, 0, 0, 0.3);
      padding: 0;
    }

    /* Specific urgency colors (.low, .normal, .critical) */
    .critical {
      border: 1px solid #${palette.base08};
    }

    .notification-content {
      background: transparent;
      padding: 6px;
      border-radius: 4px;
    }

    .close-button {
      background: @noti-close-bg;
      color: white;
      text-shadow: none;
      padding: 0;
      border-radius: 100%;
      margin-top: 10px;
      margin-right: 16px;
      box-shadow: none;
      border: none;
      min-width: 24px;
      min-height: 24px;
    }

    .close-button:hover {
      box-shadow: none;
      background: @noti-close-bg-hover;
      transition: all 0.15s ease-in-out;
      border: none;
    }

    .notification-default-action,
    .notification-action {
      padding: 4px;
      margin: 0;
      box-shadow: none;
      background: @noti-bg;
      border: 1px solid #${palette.base01};
      color: white;
    }

    .notification-default-action:hover,
    .notification-action:hover {
      -gtk-icon-effect: none;
      background: @noti-bg-hover;
    }

    .notification-default-action {
      border-radius: 4px;
    }

    /* When alternative actions are visible */
    .notification-default-action:not(:only-child) {
      border-bottom-left-radius: 0px;
      border-bottom-right-radius: 0px;
    }

    .notification-action {
      border-radius: 0px;
      border-top: none;
      border-right: none;
    }

    /* add bottom border radius to eliminate clipping */
    .notification-action:first-child {
      border-bottom-left-radius: 4px;
    }

    .notification-action:last-child {
      border-bottom-right-radius: 4px;
      border-right: 1px solid #${palette.base01};
    }

    .image {}

    .body-image {
      margin-top: 6px;
      background-color: white;
      border-radius: 12px;
    }

    .summary {
      font-size: 14px;
      font-weight: bold;
      background: transparent;
      color: white;
      text-shadow: none;
    }

    .time {
      font-size: 13px;
      font-weight: bold;
      background: transparent;
      color: white;
      text-shadow: none;
      margin-right: 18px;
    }

    .body {
      font-size: 15px;
      font-weight: normal;
      background: transparent;
      color: white;
      text-shadow: none;
    }

    /* The "Notifications" and "Do Not Disturb" text widget */
    .top-action-title {
      color: white;
      text-shadow: none;
    }

    .control-center {
      background: #${palette.base00};
      border-radius: 6px;
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

    /* Window behind control center and on all other monitors */
    .blank-window {
      background: alpha(black, 0.25);
    }

    /*** Widgets ***/

    /* Title widget */
    .widget-title {
      margin: 8px;
      font-size: 1.5rem;
    }
    .widget-title > button {
      font-size: initial;
      color: white;
      text-shadow: none;
      background: @noti-bg;
      border: 1px solid #${palette.base00};
      box-shadow: none;
      border-radius: 12px;
    }
    .widget-title > button:hover {
      background: @noti-bg-hover;
    }

    /* DND widget */
    .widget-dnd {
      margin: 8px;
      font-size: 1.1rem;
    }
    .widget-dnd > switch {
      font-size: initial;
      border-radius: 12px;
      background: @noti-bg;
      border: 1px solid #${palette.base01};
      box-shadow: none;
    }
    .widget-dnd > switch:checked {
      background: @bg-selected;
    }
    .widget-dnd > switch slider {
      background: @noti-bg-hover;
      border-radius: 12px;
    }

    /* Label widget */
    .widget-label {
      margin: 4px 180px 4px;
    }
    .widget-label > label {
      font-size: 1.1rem;
    }

    /* Mpris widget */
    .widget-mpris {}
    .widget-mpris-player {
      padding: 8px;
      margin: 8px;
    }
    .widget-mpris-title {
      font-weight: bold;
      font-size: 1.25rem;
    }
    .widget-mpris-subtitle {
      font-size: 1.1rem;
    }


    /* Menubar widget */
    .widget-menubar>box{
      border-radius: 4px 4px 0px 0px;
      background-color: @noti-bg;
    }

    .widget-menubar>box>.menu-button-bar>button{
      border: 1px solid #${palette.base01};
      background: @noti-bg;
      border-radius: 4px;
      margin: 4px 30px 4px;
      font-size: 30px;
    }

    .topbar-buttons>button { /* Name defined in config after # */
      border: none;
      background: transparent;
    }

    /* Volume widget */

    .widget-volume {
      background-color: @noti-bg;
      padding: 8px;
      margin: 8px;
      border-radius: 12px;
      font-size: 20px;
    }

    .widget-volume>box>button {
      background: transparent;
      border: none;
    }

    .per-app-volume {
      background-color: @noti-bg-alt;
      padding: 4px 8px 8px 8px;
      margin: 0px 8px 8px 8px;
      border-radius: 12px
    }

    /* Backlight widget */
    .widget-backlight {
      background-color: @noti-bg;
      padding: 8px;
      margin: 8px;
      border-radius: 12px;
      font-size: 20px;
    }

    /* Title widget */
    .widget-inhibitors {
      margin: 8px;
      font-size: 1.5rem;
    }
    .widget-inhibitors > button {
      font-size: initial;
      color: white;
      text-shadow: none;
      background: @noti-bg;
      border: 1px solid #${palette.base01};
      box-shadow: none;
      border-radius: 12px;
    }
    .widget-inhibitors > button:hover {
      background: @noti-bg-hover;
    }

    .widget-buttons-grid{
      padding: 8px;
      margin: 20px 10px 10px;
      border-radius: 12px;
      background-color: @noti-bg;
      font-size: 35px;
    }

    .widget-buttons-grid>flowbox>flowboxchild>button{
      background: @noti-bg;
      border-radius: 12px;
      margin: 3px;
    }

    .widget-buttons-grid>flowbox>flowboxchild>button:hover {
      background: @noti-bg-hover;
    }

    .screenshot-buttons,
    .screencast-buttons,
    .powermode-buttons,
    .power-buttons{
      border-radius: 4px;
    }

    .screenshot-buttons>button,
    .screencast-buttons>button,
    .powermode-buttons>button,
    .power-buttons>button{
      background: transparent;
      padding: 2px 0px;
      margin: 5px 70px 3px;
      border: 1px solid #${palette.base01};
    }

    .screenshot-buttons>button:hover,
    .screencast-buttons>button:hover,
    .powermode-buttons>button:hover,
    .power-buttons>button:hover{
      background: @noti-bg-hover;
    }
  '';
}
