{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.media.reading;
in
{
  options.valentino.modules.media.reading = {
    mdx.enable = mkEnableOption "manga support (mdx)";
    newsboat.enable = mkEnableOption "feed rss support (newsboat)";
  };

  config = mkMerge [
    (mkIf cfg.mdx.enable { home.packages = [ pkgs.mdx-go ]; })
    (mkIf cfg.newsboat.enable {
      programs.newsboat = {
        enable = true;
        # Custom wrapper to avoid un-needed flags at startup
        package = pkgs.writeShellScriptBin "newsboat" ''
          exec ${pkgs.newsboat}/bin/newsboat -q "$@"
        '';

        extraConfig = ''
          color background          default   default
          color listnormal          white     default
          color listfocus           black     blue    bold
          color listnormal_unread   green     default bold
          color listfocus_unread    black     green   bold
          color info                black     blue    bold
          color article             white     default

          highlight article "^(Feed|Title|Author|Link|Date):.*" blue    default bold
          highlight article "https?://[^ ]+"                    cyan    default underline
          highlight article "\[image [0-9]+\]"                  yellow  default bold
          error-log "~/.config/newsboat/error.log"
          cache-file "~/.config/newsboat/cache.db"
          suppress-first-reload yes

          # unbind unused keys
          unbind-key            left
          unbind-key            right
          unbind-key            J
          unbind-key            K
          unbind-key            up
          unbind-key            down
          unbind-key            C
          unbind-key            A

          # vim-like bindings
          bind-key              j   down
          bind-key              k   up
          bind-key              l   open
          bind-key              ^L  open-in-browser
          bind-key              h   quit
          bind-key              n   next
          bind-key              p   prev
          bind-key              @   show-urls
          bind-key              ^Q  hard-quit
          bind-key              !   mark-all-feeds-read
          bind-key              c   mark-feed-read
          bind-key              g   home
          bind-key              G   end
        '';

        urls =
          let
            mkUrls =
              tag: urls:
              map (entry: {
                url = entry.url;
                title =
                  entry.title or (
                    (lib.toUpper (builtins.substring 0 1 tag))
                    + (builtins.substring 1 (builtins.stringLength tag - 1) tag)
                  );
                tags = [ tag ] ++ (entry.extraTags or [ ]);
              }) urls;
          in

          builtins.concatLists [
            (mkUrls "linux" [
              {
                url = "https://www.reddit.com/r/linux.rss";
                title = "r/linux";
              }
              {
                url = "https://www.reddit.com/r/linuxmemes.rss";
                title = "r/linuxmemes";
                extraTags = [ "linux-memes" ];
              }
              {
                url = "https://www.linuxserver.io/blog.rss";
                title = "LinuxServer Blog";
                extraTags = [ "linux-server" ];
              }
            ])
            (mkUrls "nixos" [
              { url = "https://www.reddit.com/r/nixos.rss"; }
            ])
            (mkUrls "coding" [
              {
                url = "https://nullprogram.com/feed/";
                title = "Null Program";
                extraTags = [ "nullprogram" ];
              }
              {
                url = "https://www.reddit.com/r/golang.rss";
                extraTags = [ "golang" ];
              }
              {
                url = "https://bitfieldconsulting.com/posts?format=rss";
                title = "Bitfield Consulting";
                extraTags = [ "golang" ];
              }
              {
                url = "https://www.reddit.com/r/C_Programming.rss";
                title = "r/C_Programming";
                extraTags = [ "C" ];
              }
              {
                url = "https://www.reddit.com/r/ProgrammerHumor.rss";
                title = "r/ProgrammerHumor";
                extraTags = [ "meme" ];
              }
            ])
            (mkUrls "emacs" [
              { url = "https://www.reddit.com/r/emacs.rss"; }
              {
                url = "https://www.reddit.com/r/orgmode.rss";
                title = "r/orgmode";
                extraTags = [ "orgmode" ];
              }
              {
                url = "https://planet.emacslife.com/atom.xml";
                title = "Planet Emacslife";
                extraTags = [ "emacslife" ];
              }
            ])
            (mkUrls "latex" [
              { url = "https://www.reddit.com/r/LaTeX.rss"; }
            ])
            (mkUrls "android" [
              {
                url = "https://www.reddit.com/r/androiddev.rss";
                title = "r/androiddev";
                extraTags = [ "android-dev" ];
              }
            ])
            [
              {
                url = "https://selfh.st/rss/";
                title = "selfh.st";
                tags = [ "self-hosting" ];
              }
              {
                url = "https://dotfyle.com/this-week-in-neovim/rss.xml";
                title = "This Week in Neovim";
                tags = [ "neovim" ];
              }
              {
                url = "https://www.rousette.org.uk/archives/index.xml";
                title = "Geekoides";
                tags = [ "geekoides" ];
              }
              {
                url = "https://www.bytelab.codes/rss/";
                title = "Bytelab";
                tags = [ "bytelab" ];
              }
            ]
          ];
      };
    })
  ];
}
