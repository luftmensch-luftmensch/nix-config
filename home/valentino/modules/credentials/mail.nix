{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.credentials.mail-defaults;
  inherit (config.colorScheme) colors;
  channelExtraConfig = {
    Create = "Near";
    SyncState = "*";
    # Read https://vxlabs.com/2021/03/21/mbsync-copyarrivaldate-yes/
    CopyArrivalDate = "yes";
  };

  mbsyncEnabled = length (filter (a: a.mbsync.enable) (attrValues config.accounts.email.accounts)) > 0;
  msmtpEnabled = length (filter (a: a.msmtp.enable) (attrValues config.accounts.email.accounts)) > 0;

  lieerAccounts = filter (a: a.lieer.enable) (attrValues config.accounts.email.accounts);
  lieerSyncAccounts = filterAttrs (_: acc: acc.lieer.enable && acc.lieer.sync.enable) config.accounts.email.accounts;
in {
  options.valentino.modules.credentials.mail-defaults = {
    enable = mkEnableOption "mail support";
  };

  config = mkIf cfg.enable {
    accounts.email = {
      maildirBasePath = "${config.home.homeDirectory}/.config/mails";
      accounts = {
        gmail = rec {
          primary = true;
          flavor = "gmail.com";
          realName = "Valentino Bocchetti";
          address = "valentinobocchetti59@gmail.com";
          userName = address;
          passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup gmail password";

          notmuch.enable = true;
          msmtp.enable = true;
          lieer = {
            enable = true;
            sync = {
              enable = true;
              frequency = "*:0/10";
            };
            settings = {
              replace_slash_with_dot = true;
              ignore_tags = ["new" "university"];
              ignore_remote_labels = [];
            };
          };
        };
        unina = {
          primary = false;
          realName = "Valentino Bocchetti";
          userName = "vale.bocchetti@studenti.unina.it";
          address = "vale.bocchetti@studenti.unina.it";
          passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup unina password";

          imap = {
            host = "studenti.unina.it";
            port = 993;
            tls.enable = true;
          };

          smtp = {
            host = "studenti.unina.it";
            port = 465;
            tls.enable = true;
          };

          notmuch.enable = true;
          msmtp.enable = true;

          mbsync = {
            enable = true;
            subFolders = "Verbatim";

            groups.unina.channels = {
              inbox = {
                farPattern = "INBOX";
                nearPattern = "inbox";
                extraConfig =
                  {
                    Sync = "All";
                    Expunge = "Both";
                  }
                  // channelExtraConfig;
              };

              sent = {
                farPattern = "Posta inviata";
                nearPattern = "sent";
                extraConfig =
                  {
                    Sync = "All";
                    Expunge = "Both";
                  }
                  // channelExtraConfig;
              };

              drafts = {
                farPattern = "Bozze";
                nearPattern = "drafts";
                extraConfig =
                  {
                    Sync = "Pull";
                    Expunge = "Both";
                  }
                  // channelExtraConfig;
              };

              trash = {
                farPattern = "Posta eliminata";
                nearPattern = "trash";
                extraConfig =
                  {
                    Sync = "All";
                    Expunge = "None";
                  }
                  // channelExtraConfig;
              };
            };
          };
        };
      };
    };

    programs = {
      notmuch = {
        enable = true;
        new = {
          tags = ["new"];
          ignore = [];
        };

        search = {
          excludeTags = ["deleted" "spam"];
        };

        maildir.synchronizeFlags = true;

        hooks = let
          tag-and-notify = ''
            ${pkgs.afew}/bin/afew --verbose --tag --new

            SEARCH="tag:notify"
            NOTIFY_COUNT=$(${pkgs.notmuch}/bin/notmuch count "$SEARCH");

            if [ "$NOTIFY_COUNT" -gt 0 ]; then
              RESULTS=''$(${pkgs.notmuch}/bin/notmuch search --format=json --output=summary --limit=5 --sort="newest-first" "$SEARCH" | ${pkgs.jq}/bin/jq -r '.[] | "\(.authors): \(.subject)"')
              ${pkgs.libnotify}/bin/notify-send --icon=mail-unread-symbolic "$NOTIFY_COUNT New Emails:" "$RESULTS"
            fi

            ${pkgs.notmuch}/bin/notmuch tag -notify -- tag:notify
          '';
        in {
          # I let notmuch manage post-indexing stuff like this
          postNew = tag-and-notify;
        };
      };

      mbsync.enable = mbsyncEnabled;
      msmtp.enable = msmtpEnabled;
      afew = {
        enable = true;
        # I'm not assigning a tag to `gmail/archive`. I just remove the tag `inbox` if I'm archiving something.
        extraConfig = ''
          [ArchiveSentMailsFilter]
          sent_tag = sent
          [SpamFilter]
          spam_tag = spam

          # With `lieer`
          [FolderNameFilter.0]
          folder_explicit_list = gmail/mail
          folder_transforms = gmail/mail:personal
          folder_lowercases = true

          # With `imap`
          # [FolderNameFilter.0]
          # folder_explicit_list = gmail/inbox gmail/archive gmail/drafts gmail/sent gmail/trash gmail/spam
          # folder_transforms = gmail/inbox:personal gmail/archive:personal gmail/drafts:personal gmail/sent:personal gmail/trash:personal gmail/spam:personal
          # folder_lowercases = true

          # [FolderNameFilter.1]
          # folder_explicit_list = gmail/inbox gmail/drafts gmail/sent gmail/trash gmail/spam
          # folder_transforms = gmail/inbox:inbox gmail/drafts:draft gmail/sent:sent gmail/trash:deleted gmail/spam:spam
          # folder_lowercases = true

          # [FolderNameFilter.2]
          # folder_explicit_list = unina/inbox unina/drafts unina/sent unina/trash
          # folder_transforms = unina/inbox:university unina/drafts:university unina/sent:university unina/trash:university
          # folder_lowercases = true

          # [FolderNameFilter.3]
          # folder_explicit_list = unina/inbox unina/drafts unina/sent unina/trash
          # folder_transforms = unina/inbox:inbox unina/drafts:draft unina/sent:sent unina/trash:deleted
          # folder_lowercases = true

          [Filter.0]
          message = Applying notify tag and removing new tag
          query = tag:unread and tag:new and tag:inbox
          tags = +notify;-new

          [Filter.1]
          message = Removing new tag
          query = tag:new
          tags = -new
        '';
      };

      thunderbird = {
        enable = true;
        settings = {
          "privacy.donottrackheader.enabled" = true;
        };
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
                  background-color: ${colors.base00} !important;
              }
              table[is="tree-view-table"] {
                  background-color: ${colors.base02} !important;
                  color: ${colors.base01} !important;
              }
              .button {
                  background-color: ${colors.base02} !important;
              }
              .button.button-primary {
                  background-color: ${colors.base06} !important;
                  color: ${colors.base00} !important;
                  border-color: ${colors.base06} !important;
              }
              #messagePane {
                  background-color: ${colors.base00} !important;
              }
              #accountCentral {
                  background-color: ${colors.base00} !important;
              }
              #messageHeader {
                  background-color: ${colors.base02} !important;
              }
              #calendarViewHeader {
                  background-color: ${colors.base02} !important;
              }
              .calview-toggle {
                  background-color: ${colors.base00} !important;
              }
              button.calview-toggle-item {
                  background-color ${colors.base02} !important;
              }
              .minimonth-month-box,
              .minimonth-cal-box {
                  background-color: ${colors.base02} !important;
              }
              .minimonth-nav-section {
                  background-color: ${colors.base06} !important;
              }
              #folderTree:focus-within li.selected > .container, #folderTree li.drop-target > .container,
              .minimonth-week {
                  background-color: ${colors.base09} !important;
                  color: ${colors.base01} !important;
              }
              .minimonth-day[selected="true"][today="true"] {
                  background-color: ${colors.base06} !important;
                  border: 1px solid ${colors.base06} !important;
                  color: ${colors.base00} !important;
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
                  background-color: ${colors.base09} !important;
              }
              .minimonth-day[selected="true"],
              .calendar-month-day-box-current-month[selected="true"], .calendar-month-day-box-day-off[selected="true"], .calendar-month-day-box-other-month[selected="true"]{
                  border: 1px solid ${colors.base06} !important;
              }
              .calendar-month-day-box-date-label[relation="today"][selected="true"]{
                  color: ${colors.base00} !important;
              }
              .calendar-month-day-box-date-label[relation="today"] {
                  background-color: ${colors.base06} !important;
              }
              .calendar-month-day-box-day-off,
              button.calview-toggle-item,
              #agenda-toolbar,
              .tab-content {
                  background-color: ${colors.base02} !important;
              }
              .tabmail-tab:not([selected], :-moz-lwcolors),
              .new-messages > .container > .name,
              #threadTree [data-properties~="new"]:not(.selected) .thread-card-container :is(.subject, .date) {
                  color: ${colors.base06} !important;
              }
              .notification-button-container, .notification-message {
                  color: ${colors.base00} !important;
              }
              .unread > .container > .unread-count,
              .new-messages > .container > .unread-count,
              li.selected > .container,
              [is="tree-view-table-body"]:focus > .selected, [is="tree-view-table-body"]:focus-within > .selected, [is="tree-view-table-body"] > .selected:focus-within,
              .unread > .container > .unread-count, .new-messages > .container > .unread-count, li.selected > .container, button.calview-toggle-item[role="tab"][aria-selected="true"],
              button.calview-toggle-item[role="tab"][aria-selected="true"] {
                  color: ${colors.base00} !important;
                  background-color: ${colors.base06} !important;
              }
              .recipient-avatar {
                  background-color: ${colors.base00} !important;
                  color: ${colors.base06} !important;
              }
              .message-header-view-button {
                  fill: ${colors.base06 + "DD"} !important;
              }
              calendar-day-label {
                  color: ${colors.base01} !important;
                  background-color: ${colors.base00} !important;
              }
              calendar-day-label[relation="today"] {
                  color: ${colors.base06} !important;
                  background-color: ${colors.base06 + "30"} !important;
              }
              #agenda {
                  background-color: ${colors.base00}  !important;
                  --selected-background: ${colors.base06} !important;
                  --selected-foreground: ${colors.base00} !important;
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
                  background-color: ${colors.base00} !important;
              }
            '';
            userContent = ''
              .sidebar-footer-icon, .category-icon {
                  fill: ${colors.base06 + "DD"} !important;
              }
              #categories > .category[selected],
              #searchInput {
                  background-color: ${colors.base02} !important;
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
                  background-color: ${colors.base00} !important;
              }
            '';
          };
        };
      };
    };

    home.packages = with pkgs; [
      notmuch-mailmover
      tmpmail
    ];

    xdg.configFile."notmuch-mailmover/config.yaml".text = ''
      notmuch_config: ${config.home.sessionVariables.NOTMUCH_CONFIG}
      maildir: ${config.accounts.email.maildirBasePath}

      # Rename with mbsync
      rename: true
      rules:
        # gmail imap
        # - folder: gmail/archive
        #   query: tag:personal and not tag:deleted and not tag:spam and not tag:inbox
        # - folder: gmail/trash
        #   query: tag:personal and tag:deleted
        # - folder: gmail/spam
        #   query: tag:personal and tag:spam
        # - folder: gmail/inbox
        #   query: tag:personal and tag:inbox and not tag:deleted and not tag:spam
        # - folder: unina/trash
        #   query: tag:university and tag:deleted
        # - folder: unina/inbox
        #   query: tag:university and tag:inbox and not tag:deleted
    '';

    services.mbsync = {
      enable = mbsyncEnabled;
      frequency = "*:0/10";
      preExec = "${pkgs.notmuch-mailmover}/bin/notmuch-mailmover";
      postExec = "${pkgs.notmuch}/bin/notmuch new";
    };

    services.lieer.enable = true;
    systemd.user.services = mkIf config.services.lieer.enable (
      mapAttrs' (_: acc:
        nameValuePair "lieer-${acc.name}" {
          Service.ExecStartPost = "${pkgs.notmuch}/bin/notmuch new";
        })
      lieerSyncAccounts
    );

    home.activation = mkIf (lieerAccounts != []) {
      createLieerMaildir = lib.hm.dag.entryBetween ["linkGeneration"] ["writeBoundary"] ''
        $DRY_RUN_CMD mkdir -m700 -p $VERBOSE_ARG ${
          lib.concatMapStringsSep " "
          (a: "${a.maildir.absPath}/mail/{cur,new,tmp}")
          lieerAccounts
        }
      '';
    };
  };
}
