{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.credentials.mail-defaults;
  mbsyncEnabled =
    length (filter (a: a.mbsync.enable) (attrValues config.accounts.email.accounts)) > 0;
  msmtpEnabled = length (filter (a: a.msmtp.enable) (attrValues config.accounts.email.accounts)) > 0;

  lieerAccounts = filter (a: a.lieer.enable) (attrValues config.accounts.email.accounts);
  lieerSyncAccounts = filterAttrs (
    _: acc: acc.lieer.enable && acc.lieer.sync.enable
  ) config.accounts.email.accounts;
in
{
  options.valentino.modules.credentials.mail-defaults.enable = mkEnableOption "mail support";

  config = mkIf cfg.enable {
    accounts.email = {
      maildirBasePath = "${config.home.homeDirectory}/.config/mails";
      accounts.gmail = rec {
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
            ignore_tags = [
              "new"
              "university"
            ];
            ignore_remote_labels = [ ];
          };
        };
      };
    };

    programs = {
      notmuch = {
        enable = true;
        new = {
          tags = [ "new" ];
          ignore = [ ];
        };

        search.excludeTags = [
          "deleted"
          "spam"
        ];
        maildir.synchronizeFlags = true;

        hooks =
          let
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
          in
          {
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
      mapAttrs' (
        _: acc:
        nameValuePair "lieer-${acc.name}" {
          Service.ExecStartPost = "${pkgs.notmuch}/bin/notmuch new";
        }
      ) lieerSyncAccounts
    );

    home.activation = mkIf (lieerAccounts != [ ]) {
      createLieerMaildir = lib.hm.dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -m700 -p $VERBOSE_ARG ${
          lib.concatMapStringsSep " " (a: "${a.maildir.absPath}/mail/{cur,new,tmp}") lieerAccounts
        }
      '';
    };
  };
}
