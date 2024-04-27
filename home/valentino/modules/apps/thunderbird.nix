{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.apps.thunderbird;
  inherit (config.colorScheme) palette;
in {
  options.valentino.modules.apps.thunderbird.enable = mkEnableOption "thunderbird configuration";

  config = mkIf cfg.enable {
    programs.thunderbird = {
      enable = true;
      settings.privacy.donottrackheader.enabled = true;

      profiles = {
        personal = {
          isDefault = true;
          settings = {
            "mail.spellcheck.inline" = false;
            "javascript.enabled" = false;

            # General
            "general.useragent.override" = "";
            "general.warnOnAboutConfig" = false;
            "gfx.downloadable_fonts.disable_cache" = true;
            "privacy.donottrackheader.enabled" = true;
            "dom.webaudio.enabled" = false;
            "media.video_stats.enabled" = false;
            "geo.enabled" = false;
            "browser.search.geoip.timeout" = 1;

            # Disable cookie tracking and history
            "network.cookie.cookieBehavior" = 1;
            "network.cookie.lifetimePolicy" = 2;
            "network.cookie.prefsMigrated" = true;
            "places.history.enabled" = false;

            # Disable telemetry data for Mozilla
            "datareporting.policy.dataSubmissionEnabled" = false;
            "toolkit.telemetry.enabled" = false;
            "toolkit.telemetry.unified" = false;
            "toolkit.telemetry.server" = "";

            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          };

          userChrome = ''
            @namespace xul url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");
            element {
                -tab-max-width: 200px !important;
            }
            #unifiedToolbarContainer,
            #unifiedToolbar {
                display: none !important;
            }
            #threadTree[rows="thread-card"] {
                background-color: ${palette.base00} !important;
            }
            table[is="tree-view-table"] {
                background-color: ${palette.base02} !important;
                color: ${palette.base01} !important;
            }
            .button {
                background-color: ${palette.base02} !important;
            }
            .button.button-primary {
                background-color: ${palette.base06} !important;
                color: ${palette.base00} !important;
                border-color: ${palette.base06} !important;
            }
            #messagePane {
                background-color: ${palette.base00} !important;
            }
            #accountCentral {
                background-color: ${palette.base00} !important;
            }
            #messageHeader {
                background-color: ${palette.base02} !important;
            }
            #calendarViewHeader {
                background-color: ${palette.base02} !important;
            }
            .calview-toggle {
                background-color: ${palette.base00} !important;
            }
            button.calview-toggle-item {
                background-color ${palette.base02} !important;
            }
            .minimonth-month-box,
            .minimonth-cal-box {
                background-color: ${palette.base02} !important;
            }
            .minimonth-nav-section {
                background-color: ${palette.base06} !important;
            }
            #folderTree:focus-within li.selected > .container, #folderTree li.drop-target > .container,
            .minimonth-week {
                background-color: ${palette.base09} !important;
                color: ${palette.base01} !important;
            }
            .minimonth-day[selected="true"][today="true"] {
                background-color: ${palette.base06} !important;
                border: 1px solid ${palette.base06} !important;
                color: ${palette.base00} !important;
            }
            .btn-hub,
            li:not(.selected) > .container:hover,
            .tab-background[selected="true"],
            .thread-card-container:focus,
            .thread-card-container:active,
            .container:focus,
            .calendar-month-day-box-other-month,
            .calendar-month-day-box-current-month[selected="true"],
            .calendar-month-day-box-other-month.calendar-month-day-box-day-off,
            #agenda-toolbar,
            .agenda-date-header,
            tr[is="thread-card"][data-properties~="unread"]:not(.selected, :hover) {
                background-color: ${palette.base09} !important;
            }
            .minimonth-day[selected="true"],
            .calendar-month-day-box-current-month[selected="true"], .calendar-month-day-box-day-off[selected="true"], .calendar-month-day-box-other-month[selected="true"]{
                border: 1px solid ${palette.base06} !important;
            }
            .calendar-month-day-box-date-label[relation="today"][selected="true"]{
                color: ${palette.base00} !important;
            }
            .calendar-month-day-box-date-label[relation="today"] {
                background-color: ${palette.base06} !important;
            }
            .calendar-month-day-box-day-off,
            button.calview-toggle-item,
            #agenda-toolbar,
            .tab-content {
                background-color: ${palette.base02} !important;
            }
            .tabmail-tab:not([selected], :-moz-lwcolors),
            .new-messages > .container > .name,
            #threadTree [data-properties~="new"]:not(.selected) .thread-card-container :is(.subject, .date) {
                color: ${palette.base06} !important;
            }
            .notification-button-container, .notification-message {
                color: ${palette.base00} !important;
            }
            .unread > .container > .unread-count,
            .new-messages > .container > .unread-count,
            li.selected > .container,
            [is="tree-view-table-body"]:focus > .selected, [is="tree-view-table-body"]:focus-within > .selected, [is="tree-view-table-body"] > .selected:focus-within,
            .unread > .container > .unread-count, .new-messages > .container > .unread-count, li.selected > .container, button.calview-toggle-item[role="tab"][aria-selected="true"],
            button.calview-toggle-item[role="tab"][aria-selected="true"] {
                color: ${palette.base00} !important;
                background-color: ${palette.base06} !important;
            }
            .recipient-avatar {
                background-color: ${palette.base00} !important;
                color: ${palette.base06} !important;
            }
            .message-header-view-button {
                fill: ${palette.base06 + "DD"} !important;
            }
            calendar-day-label {
                color: ${palette.base01} !important;
                background-color: ${palette.base00} !important;
            }
            calendar-day-label[relation="today"] {
                color: ${palette.base06} !important;
                background-color: ${palette.base06 + "30"} !important;
            }
            #agenda {
                background-color: ${palette.base00}  !important;
                --selected-background: ${palette.base06} !important;
                --selected-foreground: ${palette.base00} !important;
            }
            .notificationbox-stack,
            :host([type="info"]) .icon,
            .list-header-bar:not([hidden]),
            .monthtable,
            .calendar-month-day-box-current-month,
            #mini-day-image,
            #today-pane-panel > .sidebar-header,
            #calendar-task-tree,
            #quick-filter-bar,
            #task-addition-box,
            #unifinder-searchBox,
            #calendar-panel,
            #calSidebar,
            #view-box,
            #tabs-toolbar,
            #toolbar-menubar,
            #statusTextBox,
            #calendar-list > li.selected,
            #folderPaneHeaderBar:not([hidden]),
            calendar-month-view, calendar-multiweek-view,
            #folderPane {
                background-color: ${palette.base00} !important;
            }
          '';
          userContent = ''
            .sidebar-footer-icon, .category-icon {
                fill: ${palette.base06 + "DD"} !important;
            }
            #categories > .category[selected],
            #searchInput {
                background-color: ${palette.base02} !important;
            }
            .sticky-container,
            .main-search,
            .main-heading,
            #content,
            #sidebar,
            #pref-category-box,
            #accountTreeBox,
            #containerBox,
            #preferencesContainer {
                background-color: ${palette.base00} !important;
            }
          '';
        };
      };
    };
  };
}
