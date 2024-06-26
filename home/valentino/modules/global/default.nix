{ pkgs, config, ... }:
let
  homeDir = "${config.home.homeDirectory}";
  browser = "firefox.desktop";
  torrent = "qbittorrent.desktop";
  image = "imv.desktop";
  video = "mpv.desktop";
  text = "nvim.desktop";
  pdf = "org.pwmt.zathura.desktop";
in
{
  nix = {
    package = pkgs.nixVersions.latest;
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      warn-dirty = false;
    };
  };

  # A systemd unit switcher for Home Manager
  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;

  # XDG_UTILS_DEBUG_LEVEL=3 xdg-mime query default <type>
  xdg = {
    mimeApps = {
      enable = true;
      associations.added = {
        "image/tiff" = [ "${image}" ];
        "image/svg+xml" = [ "${image}" ];
        "image/gif" = [ "${video}" ];
        "video/x-matroska" = [ "${video}" ];
        "video/mp4" = [ "${video}" ];
        "audio/mpeg" = [ "${video}" ];
        "application/json" = [ "${text}" ];
        "application/yaml" = [ "${text}" ];
        "application/pdf" = [ "${pdf}" ];
        "x-scheme-handler/magnet" = [ "${torrent}" ];
        "x-scheme-handler/http" = [ "${browser}" ];
        "x-scheme-handler/https" = [ "${browser}" ];
        "text/html" = [ "${text}" ];
      };
      defaultApplications = {
        "image/png" = [ "${image}" ];
        "image/jpeg" = [ "${image}" ];
        "image/jpg" = [ "${image}" ];
        "image/tiff" = [ "${image}" ];
        "image/svg+xml" = [ "${image}" ];
        "video/x-matroska" = [ "${video}" ];
        "video/mp4" = [ "${video}" ];
        "audio/mpeg" = [ "${video}" ];
        "application/json" = [ "${text}" ];
        "application/yaml" = [ "${text}" ];
        "application/pdf" = [ "${pdf}" ];
        "x-scheme-handler/magnet" = [ "${torrent}" ];
        "x-scheme-handler/http" = [ "${browser}" ];
        "x-scheme-handler/https" = [ "${browser}" ];
        "text/html" = [ "${text}" ];
      };
    };
    userDirs = {
      enable = true;
      createDirectories = true;

      # These are useless to me
      documents = null;
      publicShare = null;
      templates = null;
      pictures = null;
      music = null;

      # Curse you nemo the platypus
      desktop = "$HOME";
      download = "${homeDir}/Scaricati";
      videos = "${homeDir}/Video";
    };
  };

  # Top level session variables
  home.sessionVariables.PAGER = "bat --theme=default --paging=always --tabs=4 --wrap=never --style=plain";
}
