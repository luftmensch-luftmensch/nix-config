{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.browsers.chromium;
  inherit (config.valentino.modules) wayland;
  inherit (config.valentino.modules.credentials) bitwarden _1password;
in
{
  options.valentino.modules.browsers.chromium = {
    enable = mkEnableOption "chromium";
    ungoogled = mkEnableOption "UnGoogled features";
  };

  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      # chrome://flags/#enable-webrtc-pipewire-capturer (Enable it to share entire screen)
      commandLineArgs =
        [
          "--force-dark-mode"
          "--gtk-version=4"
          "--no-first-run"
          "--no-default-browser-check"
          "--no-service-autorun"
          "--enable-features=WebUIDarkMode"
          "--extension-content-verification=enforce_strict"
          "--extensions-install-verification=enforce_strict"
          "--disk-cache=$XDG_RUNTIME_DIR/chromium-cache"

          "--ignore-gpu-blocklist"
          "--enable-oop-rasterization"
          "--enable-gpu-rasterization"
          "--enable-zero-copy"
        ]
        ++ optionals wayland.enable [
          "--ozone-platform=wayland"
          "--enable-usermedia-screen-capturing"

          "--disable-features=UseChromeOSDirectVideoDecoder"
          "--enable-media-router"
          "--enable-smooth-scrolling"
        ]
        ++ optionals cfg.ungoogled [
          "--disable-search-engine-collection"
          "--extension-mime-request-handling=always-prompt-for-install"
          "--fingerprinting-canvas-image-data-noise"
          "--fingerprinting-canvas-measuretext-noise"
          "--fingerprinting-client-rects-noise"
          "--popups-to-tabs"
          "--force-punycode-hostnames"
          "--show-avatar-button=incognito-and-guest"
          "--no-pings"
          "--disable-breakpad"
          # I don't need these, thus I disable them
          "--disable-speech-api"
          "--disable-speech-synthesis-api"
          "--disable-sync"
          "--disable-reading-from-canvas"

          (
            "--enable-features="
            + lib.strings.concatMapStrings (x: x + ",") [
              # Enable visited link database partitioning
              "PartitionVisitedLinkDatabase"
              # Enable prefetch privacy changes
              "PrefetchPrivacyChanges"
              # Enable split cache
              "SplitCacheByNetworkIsolationKey"
              "SplitCodeCacheByNetworkIsolationKey"
              # Enable partitioning connections
              "EnableCrossSiteFlagNetworkIsolationKey"
              "HttpCacheKeyingExperimentControlGroup"
              "PartitionConnectionsByNetworkIsolationKey"
              # Enable strict origin isolation
              "StrictOriginIsolation"
              # Enable reduce accept language header
              "ReduceAcceptLanguage"
              # Enable content settings partitioning
              "ContentSettingsPartitioning"
            ]
          )

          (
            "--disable-features="
            + lib.strings.concatMapStrings (x: x + ",") [
              # Disable autofill
              "AutofillPaymentCardBenefits"
              "AutofillPaymentCvcStorage"
              "AutofillPaymentCardBenefits"
              # Disable third-party cookie deprecation bypasses
              "TpcdHeuristicsGrants"
              "TpcdMetadataGrants"
              # Disable hyperlink auditing
              "EnableHyperlinkAuditing"
              # Disable showing popular sites
              "NTPPopularSitesBakedInContent"
              "UsePopularSitesSuggestions"
              # Disable article suggestions
              "EnableSnippets"
              "ArticlesListVisible"
              "EnableSnippetsByDse"
              # Disable content feed suggestions
              "InterestFeedV2"
              # Disable media DRM preprovisioning
              "MediaDrmPreprovisioning"
              # Disable autofill server communication
              "AutofillServerCommunication"
              # Disable new privacy sandbox features
              "PrivacySandboxSettings4"
              "BrowsingTopics"
              "BrowsingTopicsDocumentAPI"
              "BrowsingTopicsParameters"
              # Disable translate button
              "AdaptiveButtonInTopToolbarTranslate"
              # Disable detailed language settings
              "DetailedLanguageSettings"
              # Disable fetching optimization guides
              "OptimizationHintsFetching"
              # Partition third-party storage
              "DisableThirdPartyStoragePartitioningDeprecationTrial2"

              # Disable media engagement
              "PreloadMediaEngagementData"
              "MediaEngagementBypassAutoplayPolicies"
            ]
          )
        ];

      extensions =
        [
          { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock
          { id = "iaiomicjabeggjcfkbimgmglanimpnae"; } # tab manager
          { id = "inojafojbhdpnehkhhfjalgjjobnhomj"; } # tempmail
          { id = "fonalplhodhnenmokepaijoemaednpjm"; } # directo
        ]
        ++ (lib.optionals bitwarden.enable) [ { id = "nngceckbapebfimnlniiiahkandclblb"; } ]
        ++ (lib.optionals _1password.enable) [ { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } ];
    };
  };
}
