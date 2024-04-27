{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.browsers.chromium;
  inherit (config.valentino.modules) themes wayland;
  configDir = "${config.home.homeDirectory}/nix-config/home/valentino/modules/browsers";
  userChrome = ''
    * {
      --animation-speed: 0.2s;
      --button-corner-rounding: 30px;
      --urlbar-container-height: 40px !important;
      --urlbar-min-height: 30px !important;
      --urlbar-height: 30px !important;
      --urlbar-toolbar-height: 38px !important;
      --moz-hidden-unscrollable: scroll !important;
      --toolbarbutton-border-radius: 3px !important;
      --tabs-border-color: transparent;
    }

    :root {
        --window: -moz-Dialog !important;
        --secondary: color-mix(in srgb, currentColor 5%, -moz-Dialog) !important;
        --uc-border-radius: 0px;
        --uc-status-panel-spacing: 0px;
        --uc-page-action-margin: 7px;
    }

    /* animation and effect */
    #nav-bar:not([customizing]) {
      visibility: visible;
      margin-top: -40px;
      transition-delay: 0.1s;
      filter: alpha(opacity=0);
      opacity: 0;
      transition: visibility, ease 0.1s, margin-top, ease 0.1s, opacity, ease 0.1s,
      rotate, ease 0.1s !important;
    }

    #nav-bar:hover,
    #nav-bar:focus-within,
    #urlbar[focused='true'],
    #identity-box[open='true'],
    #titlebar:hover + #nav-bar:not([customizing]),
    #toolbar-menubar:not([inactive='true']) ~ #nav-bar:not([customizing]) {
      visibility: visible;

      margin-top: 0px;
      filter: alpha(opacity=100);
      opacity: 100;
      margin-bottom: -0.2px;
    }
    #PersonalToolbar {
      margin-top: 0px;
    }
    #nav-bar .toolbarbutton-1[open='true'] {
      visibility: visible;
      opacity: 100;
    }

    :root:not([customizing]) :hover > .tabbrowser-tab:not(:hover) {
      transition: blur, ease 0.1s !important;
    }

    :root:not([customizing]) :not(:hover) > .tabbrowser-tab {
      transition: blur, ease 0.1s !important;
    }

    #tabbrowser-tabs .tab-label-container[customizing] {
      color: transparent;
      transition: ease 0.1s;
      transition-delay: 0.2s;
    }

    .tabbrowser-tab:not([pinned]) .tab-icon-image ,.bookmark-item .toolbarbutton-icon{opacity: 0!important; transition: .15s !important; width: 0!important; padding-left: 16px!important}
    .tabbrowser-tab:not([pinned]):hover .tab-icon-image,.bookmark-item:hover .toolbarbutton-icon{opacity: 100!important; transition: .15s !important; display: inline-block!important; width: 16px!important; padding-left: 0!important}
    .tabbrowser-tab:not([hover]) .tab-icon-image,.bookmark-item:not([hover]) .toolbarbutton-icon{padding-left: 0!important}

    /*  Removes annoying buttons and spaces */
    .titlebar-spacer[type="pre-tabs"], .titlebar-spacer[type="post-tabs"]{display: none !important}
    #tabbrowser-tabs{border-inline-start-width: 0!important}

    /*  Makes some buttons nicer  */
    #PanelUI-menu-button, #unified-extensions-button, #reload-button, #stop-button {padding: 2px !important}
    #reload-button, #stop-button{margin: 1px !important;}

    /* X-button */
    :root {
        --show-tab-close-button: none;
        --show-tab-close-button-hover: -moz-inline-block;
    }
    .tabbrowser-tab:not([pinned]) .tab-close-button { display: var(--show-tab-close-button) !important; }
    .tabbrowser-tab:not([pinned]):hover .tab-close-button { display: var(--show-tab-close-button-hover) !important }

    /* tabbar */

    /* Hide the secondary Tab Label
     * e.g. playing indicator (the text, not the icon) */
    .tab-secondary-label { display: none !important; }

    :root {
      --toolbarbutton-border-radius: 0 !important;
      --tab-border-radius: 0 !important;
      --tab-block-margin: 0 !important;
    }

    .tabbrowser-tab:is([visuallyselected='true'], [multiselected])
      > .tab-stack
      > .tab-background {
      box-shadow: none !important;
    }

    .tabbrowser-tab[last-visible-tab='true'] {
      padding-inline-end: 0 !important;
    }

    #tabs-newtab-button {
      padding-left: 0 !important;
    }

    /* multi tab selection */
    #tabbrowser-tabs:not([noshadowfortests]) .tabbrowser-tab:is([multiselected])
      > .tab-stack
      > .tab-background:-moz-lwtheme { outline-color: var(--toolbarseparator-color) !important; }

    /* remove gap after pinned tabs */
    #tabbrowser-tabs[haspinnedtabs]:not([positionpinnedtabs])
      > #tabbrowser-arrowscrollbox
      > .tabbrowser-tab:nth-child(1 of :not([pinned], [hidden])) { margin-inline-start: 0 !important; }

    /*  Removes annoying border  */
    #navigator-toolbox{border:none !important;}

    /*  Removes the annoying rainbow thing from the hamburger  */
    #appMenu-fxa-separator{border-image:none !important;}
  '';

  userContent = ''
    @-moz-document url-prefix(about:){

    /*  Removes the scrollbar on some places  */
    body,html{overflow-y: auto}

    /*  Devtools  */
    @-moz-document url-prefix(about:devtools){
    #toolbox-container{margin-top: 10px !important}
    .devtools-tabbar{background: transparent !important}
    .devtools-tab-line{border-radius: 0 0 5px 5px}
    .customize-animate-enter-done,.customize-menu,.top-site-outer:hover,button{background-color: transparent!important}}

    /*  Newtab  */
    @-moz-document url("about:home"), url("about:newtab"){
    .search-wrapper .search-handoff-button .fake-caret {top: 13px !important; inset-inline-start: 48px !important}
    .search-wrapper .logo-and-wordmark{opacity: 0.9 !important; order: 1 !important; margin-bottom: 0 !important; flex: 1 !important; flex-basis: 20% !important}
    .search-wrapper .search-handoff-button .fake-caret{top: 13px !important; inset-inline-start: 48px !important}
    .search-wrapper .logo-and-wordmark{opacity: 0.9 !important; order: 1 !important; margin-bottom: 0 !important; flex: 1 !important; flex-basis: 20% !important}
    .outer-wrapper .search-wrapper{padding: 0px !important; display: flex !important; flex-direction: row !important; flex-wrap: wrap !important; justify-content: center !important; align-items: center !important; align-content: space-around !important; gap: 20px 10px !important}
    .search-wrapper .logo-and-wordmark .logo{background-size: 60px !important; height: 60px !important; width: 60px !important}
    .search-wrapper .search-inner-wrapper{min-height: 42px !important; order: 2 !important; flex: 3 !important; flex-basis: 60% !important; top: 4px !important}
    .search-wrapper .search-inner-wrapper{min-height: 42px !important; order: 2 !important; flex: 3 !important; flex-basis: 60% !important; top: 4px !important}
    .outer-wrapper.ds-outer-wrapper-breakpoint-override.only-search.visible-logo{display: flex !important; padding-top: 0px !important;vertical-align: middle}
    .customize-menu{border-radius: 10px 0 0 10px !important}
    #root > div{align-items: center; display: flex}}}

  '';
in {
  options.valentino.modules.browsers.firefox.enable = mkEnableOption "firefox";

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      package =
        if wayland.enable
        then pkgs.firefox-wayland
        else pkgs.firefox;

      profiles.default = {
        inherit userChrome userContent;
        name = "Default";
        search = {
          force = true;
          default = "DuckDuckGo";
          engines = {
            "DuckDuckGo".metaData.alias = "@d";
            "Google".metaData = {
              alias = "@g";
              hidden = true;
            };
            "Bing".metaData.hidden = true;
            "eBay".metaData.hidden = true;
            "Wikipedia".metaData.hidden = true;
            "Nix Packages" = {
              urls = [{template = "https://search.nixos.org/packages?channel=unstable&query={searchTerms}";}];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = ["@np"];
            };
          };
        };
        #                       Disable automatic downloading of OpenH264 codec
        #  1. https://support.mozilla.org/en-US/kb/how-stop-firefox-making-automatic-connections#w_media-capabilities
        #  2. https://andreasgal.com/2014/10/14/openh264-now-in-firefox/

        #                       Hardware video acceleration
        #  1. https://wiki.archlinux.org/title/Firefox#Hardware_video_acceleration

        #    Disable sending reports of tab crashes to Mozilla (about:tabcrashed), don't nag user about unsent crash reports
        # 1. https://hg.mozilla.org/mozilla-central/file/tip/browser/app/profile/firefox.js

        #                      Privacy (Enable Firefox Tracking Protection)
        # 1. https://wiki.mozilla.org/Security/Tracking_protection
        # 2. https://support.mozilla.org/en-US/kb/tracking-protection-firefox
        # 3. https://support.mozilla.org/en-US/kb/tracking-protection-pbm
        # 4. https://kontaxis.github.io/trackingprotectionfirefox/
        # 5. https://feeding.cloud.geek.nz/posts/how-tracking-protection-works-in-firefox/

        #                     Firefox's anti-fingerprinting mode ("resist fingerprinting" or RFP) (Tor Uplift project)
        # 1. https://wiki.mozilla.org/Security/Tor_Uplift/Tracking
        # 2. https://bugzilla.mozilla.org/show_bug.cgi?id=1333933
        # 3. https://wiki.mozilla.org/Security/Fingerprinting
        # NOTICE: RFP breaks some keyboard shortcuts used in certain websites
        # NOTICE: RFP changes your time zone

        #                     Contextual identity Containers feature
        # NOTICE: Containers are not available in Private Browsing mode
        # 1. https://wiki.mozilla.org/Security/Contextual_Identity_Project/Containers

        #                     Disable speculative pre-connections
        # 1. https://support.mozilla.org/en-US/kb/how-stop-firefox-making-automatic-connections#w_speculative-pre-connections
        # 2. https://bugzilla.mozilla.org/show_bug.cgi?id=814169

        #                     Disable disk cache (Do not download URLs for the offline cache )
        # 1. http://kb.mozillazine.org/Browser.cache.offline.enable
        # 2. http://kb.mozillazine.org/Browser.cache.disk.enable

        #                     Useful custom user.js:
        # 1. https://github.com/pyllyukko/user.js

        #                     Firefox Hardening Guide
        # 1. https://brainfucksec.github.io/firefox-hardening-guide
        settings = {
          "browser.uidensity" = 0;
          "svg.context-properties.content.enabled" = true;

          "network.trr.mode" = 2;
          "network.trr.uri" = "https://dns.quad9.net/dns-query";
          "beacon.enabled" = false;

          # BREAK PAD
          # Disable sending Firefox crash reports to Mozilla servers
          # A list of submitted crash reports can be found at about:crashes
          "breakpad.reportURL" = "";

          # BROWSER - BOOKMARKS
          "browser.bookmarks.autoExportHTML" = true;
          #"browser.bookmarks.showMobileBookmarks" = true;

          # BROWSER - CACHE

          # Disable the Offline Cache.
          # Websites can store up to 500 MB of data in an offline cache (http://kb.mozillazine.org/Browser.cache.offline.enable= to be able to run even
          # when there is no working internet connection. This could possibly be used to store an user id.

          #"browser.cache.offline.enable"= false;
          #"browser.cache.disk.enable"= false;

          # BROWSER - CRASH REPORTS
          #"browser.crashReports.unsubmittedCheck.enabled"= false;

          # BROWSER - CHECK DEFAULT
          "browser.shell.checkDefaultBrowser" = false;

          # BROWSER - DISCOVERY
          "browser.discovery.enabled" = false;

          # BROWSER - DISPLAY
          #"browser.display.background_color" = "";
          #"browser.display.foreground_color" = "";
          #"browser.display.document_color_use" = 2;
          #"browser.anchor_color" = "";
          #"browser.visited_color" = "",

          # BROWSER - DOWNLOAD
          # Disable Downloading on Desktop
          #"browser.download.folderList"= 2;

          # BROWSER - FIXUP
          # Disable location bar domain guessing
          "browser.fixup.alternate.enabled" = false;

          # BROWSER - HTTP Cert
          "browser.xul.error_pages.expert_bad_cert" = true;

          # BROWSER - OPTIONS
          "browser.send_pings" = false;

          # BROWSER - REGION
          "browser.region.network.url" = "";
          "browser.region.update.enabled" = false;

          # BROWSER - SAFEBROWSING
          "browser.safebrowsing.downloads.remote.enabled" = false;

          # turn off google safebrowsing (it literally sends a sha sum of everything you download to google
          "browser.safebrowsing.appRepURL" = "";
          "browser.safebrowsing.downloads.remote.block_dangerous" = false;
          "browser.safebrowsing.downloads.remote.block_dangerous_host" = false;
          "browser.safebrowsing.downloads.remote.block_potentially_unwanted" = false;
          "browser.safebrowsing.downloads.remote.block_uncommon" = false;

          # Google safebrowsing can detect phishing and malware but it also sends
          # informations to google together with an unique id called wrkey
          "browser.safebrowsing.downloads.remote.url" = "";
          "browser.safebrowsing.downloads.enabled" = false;
          "browser.sessionstore.privacy_level" = 2;
          # Reduce File IO / SSD abuse
          # Otherwise= Firefox bombards the HD with writes. Not so nice for SSDs.
          # This forces it to write every 30 minutes= rather than 15
          "browser.sessionstore.interval" = "1800000";
          "browser.safebrowsing.provider.google4.gethashURL" = "";
          "browser.safebrowsing.provider.google4.updateURL" = "";
          "browser.safebrowsing.provider.google.gethashURL" = "";
          "browser.safebrowsing.provider.google.updateURL" = "";

          # BROWSER - SEARCH
          "browser.search.region" = "IT";
          "browser.search.selectedEngine" = "DuckDuckGo";
          "browser.search.defaultenginename" = "DuckDuckGo";
          "browser.search.hiddenOneOffs" = "Yandex,Amazon.com,Bing,Google,Wikipedia (en";

          # Disable Search Suggestions
          # Firefox suggests search terms in the search field. This will send everything
          # typed or pasted in the search field to the chosen search engine= even when you
          # did not press enter.
          #"browser.search.suggest.enabled" = true;

          # BROWSER - TABS
          "browser.tabs.closeWindowWithLastTab" = true;
          "browser.tabs.crashReporting.sendReport" = false;

          "browser.newtab.preload" = false;
          "browser.newtabpage.enhanced" = false;
          "browser.newtabpage.directory.ping" = "";
          "browser.newtabpage.directory.source" = "data:text/plain,{}";
          "browser.newtabpage.activity-stream.telemetry" = false;
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.newtabpage.activity-stream.feeds.topsites" = false;

          "browser.newtabpage.activity-stream.showSponsored" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          # clear default topsites ([NOTE] This does not block you from adding your own)
          "browser.newtabpage.activity-stream.default.sites" = "";

          # iirc hides pocket stories
          "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
          # Pocket Reading List
          "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;

          # Disable Sponsored Top Sites
          # Firefox 83 introduced sponsored top sites
          # (https://support.mozilla.org/en-US/kb/sponsor-privacy= which are sponsored ads
          # displayed as suggestions in the URL bar.
          "services.sync.prefs.sync.browser.newtabpage.activity-stream.showSponsoredTopSite" = false;

          "browser.ping-centre.telemetry" = false;

          # BROWSER - UI
          "browser.uitour.enabled" = false;
          "browser.startup.homepage" = "file:///home/valentino/.config/startpage.html";

          # Disable the first run tabs with advertisements for the latest firefox features.
          #"browser.startup.homepage_override.mstone" = "ignore";

          # BROWSER - URL
          "browser.urlbar.trimURLs" = true;
          # Disable inline autocomplete in URL bar
          #"browser.urlbar.autoFill"= false;
          #"browser.urlbar.autoFill.typed"= false;

          #'browser.urlbar.placeholderName'= 'Google';
          #'browser.urlbar.placeholderName.private'= 'Google';

          # Captive Portal Detection - Network Checks
          "captivedetect.canonicalURL" = "";

          # DATA REPORTING

          # Disable health report
          # Disable sending Firefox health reports
          # (https://www.mozilla.org/privacy/firefox/#health-report to Mozilla
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.healthreport.service.enabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;

          # DEV TOOLS
          "devtools.toolbox.host" = "left";
          "devtools.theme" = "dark";
          "devtools.onboarding.telemetry.logged" = false;
          "devtools.inspector.three-pane-enabled" = false;

          #"devtools.chrome.enabled"= true;
          #"devtools.debugger.remote-enabled"= true;
          #"devtools.responsive.touchSimulation.enabled"= true;

          # DOM
          # Disable that websites can get notifications if you copy= paste, or cut something from a web page, and it lets them know which part of the page had been selected.
          "dom.battery.enabled" = false;
          "dom.event.clipboardevents.enabled" = false;
          "dom.gamepad.enabled" = false;
          "dom.security.https_only_mode" = true;
          #"dom.security.https_only_mode_ever_enabled" = true;

          # Disable DOM storage
          # Disables DOM storage= which enables so called "supercookies". Some modern sites
          # will not work (i.e. missing "save" functions.
          #"dom.storage.enabled" = true;

          # Disable IndexedDB (breaks things
          # abused for tracking (http://www.w3.org/TR/IndexedDB/">IndexedDB</a> is a way=
          # websites can store structured data. This can be <a
          # href="http://arstechnica.com/apple/2010/09/rldguid-tracking-cookies-in-safari-database-form/=
          # too. Disabling causes problems when sites depend on it like Tweetdeck or Reddit
          # and extensions that use it to store their data. Some users reported crashing
          # tabs when IndexedDB is disabled. Only disable it= when you know what you're
          # doing.
          #"dom.indexedDB.enabled" = true;

          # EXPERIMENTS
          "experiments.supported" = false;
          "experiments.enabled" = false;
          "experiments.manifest.uri" = "";
          "experiments.activeExperiment" = false;

          # EXTENSIONS
          "extensions.getAddons.showPane" = false;
          # Opt out metadata updates
          # Firefox sends data about installed addons as metadata updates
          # (https://blog.mozilla.org/addons/how-to-opt-out-of-add-on-metadata-updates/= so
          # Mozilla is able to recommend you other addons.
          #"extensions.getAddons.cache.enabled" = false;

          "extensions.htmlaboutaddons.recommendations.enabled" = false;
          "extensions.htmlaboutaddons.discover.enabled" = false;
          "extensions.pocket.enabled" = false;
          "extensions.screenshots.disabled" = true;
          "extensions.shield-recipe-client.enabled" = false;
          "extensions.unifiedExtensions.enabled" = false;

          /*
          * TAB BEHAVIOR *
          */
          "findbar.highlightAll" = true;

          # FONTS
          "font.name.monospace.x-western" = "${themes.font.regular.family}";
          "font.name.sans-serif.x-western" = "${themes.font.regular.family}";
          "font.name.serif-x-western" = "${themes.font.regular.family}";

          # GEO Preference
          "geo.provider.network.url" = "https:#location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";
          "geo.provider.use_gpsd" = false;

          # GFX - Webrender
          #"gfx.webrender.all" = true;

          # LAYOUT
          "layout.display-list.retain" = true;
          "layout.display-list.retain.chrome" = true;

          # LAYERS
          "layers.acceleration.force-enabled" = true;
          "layers.omtp.enabled" = true;

          # LOOP
          #"loop.logDomains" = false;

          # MEDIA
          # Enable WebRTC VA-API decoding support
          "media.ffmpeg.vaapi.enabled" = true;
          #"media.ffvpx.enabled" = true;
          #"media.navigator.mediadatadecoder_vpx_enabled" = true;
          #"media.gmp-gmpopenh264.enabled"= false;
          #"media.gmp-manager.url"= "";

          # Disable media device queries
          # Prevent websites from accessing information about webcam and microphone
          # (https://developer.mozilla.org/docs/Web/API/MediaDevices/enumerateDevices
          # (possible fingerprinting.
          #"media.navigator.enabled" = false;

          # Disable video statistics
          # Prevent websites from measuring video performance (possible fingerprinting. See
          # Mozilla Bug 654550 (https://bugzilla.mozilla.org/show_bug.cgi?id=654550.
          #media.video_stats.enabled" = false;

          # NETWORK
          # Not rendering IDNs as their Punycode equivalent leaves you open to phishing attacks that can be very difficult to notice.
          "network.IDN_show_punycode" = true;
          # Disable Firefox prefetching pages it thinks you will visit next:
          # Prefetching causes cookies from the prefetched site to be loaded and other potentially unwanted behavior. Details here and here.
          #"network.dns.disablePrefetch" = true;
          #"network.dns.disablePrefetchFromHTTPS" = true;
          #"network.predictor.enabled" = false;
          #"network.predictor.enable-prefetch" = false;
          #"network.prefetch-next" = false;
          #"network.http.speculative-parallel-limit"= 0;
          # third party cookies
          #"network.cookie.cookieBehavior" = 1;
          "network.connectivity-service.enabled" = false;
          "network.captive-portal-service.enabled" = false;
          #"network.http.referer.XOriginPolicy"= 2;
          #"network.http.referer.XOriginTrimmingPolicy"= 2;

          # Block Referer
          # Firefox tells a website= from which site you're coming (the so called RefControl
          # (http://kb.mozillazine.org/Network.http.sendRefererHeader">referer</a>. You can
          # find more detailed settings in this <a
          # href="http://www.ghacks.net/2015/01/22/improve-online-privacy-by-controlling-referrer-information/">ghacks
          # article</a> or install the <a
          # href="https://addons.mozilla.org/firefox/addon/refcontrol/ extension for per
          # domain settings.
          #"network.http.referer.spoofSource" = true;

          # NORMANDY & SHIELDS

          # Disable shield studies (telemetry feature
          # Mozilla shield studies (https://wiki.mozilla.org/Firefox/Shield is a feature which allows mozilla to remotely install experimental addons.
          "app.normandy.enabled" = false;
          "app.normandy.api_url" = "";
          "app.shield.optoutstudies.enabled" = false;

          # Disable automatic updates.
          # Updates are no longer installed automatically. You will still be notified when
          # an update is available and can install it. Avoids getting a new (maybe addon incompatible version.
          "app.update.auto" = false;

          # PLACES
          # Don't remember browsing history
          #"places.history.enabled" = false;

          # PDF
          # Disable internal firefox pdf viewer (Disable Javascript in PDF viewer
          # Disables executing of JavaScript in the PDF form viewer. It is possible that some PDFs are not rendered correctly due to missing functions.
          #"pdfjs.disabled" = true;

          # PRIVACY
          #"privacy.trackingprotection.enabled"= true;
          #"privacy.trackingprotection.pbmode.enabled"= true;
          #"privacy.trackingprotection.cryptomining.enabled" = true;
          #"privacy.trackingprotection.fingerprinting.enabled" = true;
          #"privacy.trackingprotection.socialtracking.enabled" = true;

          #"privacy.userContext.enabled"= true;
          #"privacy.resistFingerprinting"= true;
          #"privacy.sanitize.sanitizeOnShutdown"= true;
          #"privacy.clearOnShutdown.cache"= true;
          #"privacy.clearOnShutdown.cookies"= true;
          #"privacy.clearOnShutdown.downloads"= true;
          #"privacy.clearOnShutdown.formdata"=	true;
          #"privacy.clearOnShutdown.history"= true;
          #"privacy.clearOnShutdown.offlineApps"= true;
          #"privacy.clearOnShutdown.sessions"= true;
          #"privacy.clearOnShutdown.openWindows"= true;

          # Enable firstparty isolation.
          # FPI works by separating cookies on a per-domain basis. In this way tracking
          # networks won't be able to locate the same cookie on different sites. Note that this might break third-party logins.
          #"privacy.firstparty.isolate" = false;

          # Enable query parameter stripping
          # Firefox 102 introduced query parameter stripping like utm_source. Enabled by
          # default with Strict Enhanced Tracking Protection.
          #"privacy.query_stripping" = true;

          # READER
          "reader.color_scheme" = "dark"; # or sepia

          # SECURITY

          # Show in-content login form warning UI for insecure login fields
          # "security.insecure_field_warning.contextual.enabled"= true;

          # PREF: Show in-content login form warning UI for insecure login fields
          # "security.insecure_field_warning.contextual.enabled"= true;

          # Yubikey
          #"security.webauth.u2f" = true;
          #"security.webauth.webauthn" = true;
          #"security.webauth.webauthn_enable_softtoken" = true;
          #"security.webauth.webauthn_enable_usbtoken" = true;
          #"security.pki.sha1_enforcement_level"= 1;

          "security.ssl.errorReporting.automatic" = false;
          "security.tls.enable_0rtt_data" = false;

          # SIGNON
          # Disable internal password manager
          "signon.rememberSignons" = false;

          # Require manual intervention to autofill known username/passwords sign-in forms
          "signon.autofillForms" = false;

          # Disable formless login capture
          #"signon.formlessCapture.enabled"= false;

          # When username/password autofill is enabled= still disable it on non-HTTPS sites
          #"signon.autofillForms.http"= false;

          # TOOLKIT
          # Customizations
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.bhrPing.enabled" = false;
          "toolkit.coverage.endpoint.base" = "";
          "toolkit.coverage.opt-out" = true;
          "toolkit.telemetry.coverage.opt-out" = true;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.firstShutdownPing.enabled" = false;
          "toolkit.telemetry.newProfilePing.enabled" = false;
          "toolkit.telemetry.shutdownPingSender.enabled" = false;
          "toolkit.telemetry.server" = "";
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.updatePing.enabled" = false;

          "toolkit.telemetry.cachedClientID" = "";
          #"toolkit.telemetry.hybridContent.enabled" = false;
          #"toolkit.telemetry.reportingpolicy.firstRun" = false;

          # UI
          # Avoid open directly a link with right click
          "ui.context_menus.after_mouseup" = true;
          # "extensions.activeThemeID"= "firefox-compact-dark@mozilla.org"; // default default-theme@mozilla.org

          # Moved here to be able to know about error in the configuration
          "browser.aboutConfig.showWarning" = false;
        };

        extensions = with pkgs.nur.repos.rycee.firefox-addons;
          [bitwarden ublock-origin user-agent-string-switcher]
          # https://gitlab.com/magnolia1234/bypass-paywalls-firefox-clean (alternative to https://12ft.io/)
          ++ (with pkgs.nur.repos.colinsane.firefox-extensions; [bypass-paywalls-clean]);
      };
    };

    home.file.".config/startpage.html".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/startpage.html";
  };
}
