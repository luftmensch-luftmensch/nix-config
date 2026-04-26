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
    izrss.enable = mkEnableOption "feed rss support (izrss)";
  };

  config = mkMerge [
    (mkIf cfg.mdx.enable { home.packages = [ pkgs.mdx-go ]; })
    (mkIf cfg.izrss.enable { home.packages = [ pkgs.izrss ]; })
    (mkIf cfg.newsboat.enable {
      programs.newsboat =
        let
          linux = "linux";
          nixos = "nixos";
          coding = "coding";
          emacs = "emacs";
          android = "android";

          mkUrls =
            tag: urls:
            map (entry: {
              url = entry.url;
              tags = [ tag ] ++ (entry.extraTags or [ ]);
            }) urls;
        in
        {
          enable = true;
          urls = builtins.concatLists [
            (mkUrls linux [
              { url = "https://www.reddit.com/r/linux.rss"; }
              {
                url = "https://www.reddit.com/r/linuxmemes.rss";
                extraTags = [ "linux-memes" ];
              }
              {
                url = "https://www.linuxserver.io/blog.rss";
                extraTags = [ "linux-server" ];
              }
            ])

            (mkUrls nixos [
              { url = "https://www.reddit.com/r/nixos.rss"; }
              {
                url = "https://christine.website/blog.rss";
                extraTags = [ "Xe" ];
              }
            ])

            (mkUrls coding [
              {
                url = "https://nullprogram.com/feed/";
                extraTags = [ "nullprogram" ];
              }
              {
                url = "https://www.reddit.com/r/golang.rss";
                extraTags = [ "golang" ];
              }
              {
                url = "https://bitfieldconsulting.com/golang?format=rss";
                extraTags = [ "golang" ];
              }
              {
                url = "https://www.reddit.com/r/C_Programming.rss";
                extraTags = [ "C" ];
              }
              {
                url = "https://www.reddit.com/r/ProgrammerHumor.rss";
                extraTags = [ "meme" ];
              }
              {
                url = "https://vishnubharathi.codes/atom.xml";
                extraTags = [ "vishnubharathi" ];
              }
            ])

            (mkUrls emacs [
              { url = "https://www.reddit.com/r/emacs.rss"; }
              {
                url = "https://www.reddit.com/r/orgmode.rss";
                extraTags = [ "orgmode" ];
              }
              {
                url = "https://planet.emacslife.com/atom.xml";
                extraTags = [ "emacslife" ];
              }
            ])

            (mkUrls "latex" [
              { url = "https://www.reddit.com/r/LaTeX.rss"; }
            ])

            (mkUrls android [
              {
                url = "https://www.reddit.com/r/androiddev.rss";
                extraTags = [ "android-dev" ];
              }
              {
                url = "https://www.reddit.com/r/fdroid.rss";
                extraTags = [ "fdroid" ];
              }
              {
                url = "https://www.reddit.com/r/FlutterDev.rss";
                extraTags = [ "flutter" ];
              }
              {
                url = "https://codewithandrea.com/rss.xml";
                extraTags = [ "flutter" ];
              }
            ])

            [
              {
                url = "https://selfh.st/rss/";
                tags = [ "self-hosting" ];
              }
              {
                url = "https://dotfyle.com/this-week-in-neovim/rss.xml";
                tags = [ "neovim" ];
              }
              {
                url = "https://www.rousette.org.uk/archives/index.xml";
                tags = [ "geekoides" ];
              }
              {
                url = "https://www.bytelab.codes/rss/";
                tags = [ "bytelab" ];
              }
            ]
          ];
        };
    })
  ];
}
