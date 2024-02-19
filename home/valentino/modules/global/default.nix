{
  pkgs,
  config,
  lib,
  ...
}: let
  homeDir = "${config.home.homeDirectory}";
  browser = "firefox.desktop";
  torrent = "qbittorrent.desktop";
  image = "imv.desktop";
  video = "mpv.desktop";
  text = "nvim.desktop";
  pdf = "org.pwmt.zathura.desktop";
in {
  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = ["nix-command" "flakes" "repl-flake"];
      warn-dirty = false;
    };
  };

  # A systemd unit switcher for Home Manager
  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;

  xdg = {
    mimeApps = {
      enable = true;
      associations.added = {
        "image/png" = ["${image}"];
        "image/jpeg" = ["${image}"];
        "image/tiff" = ["${image}"];
        "image/svg+xml" = ["${image}"];
        "image/gif" = ["${video}"];
        "video/x-matroska" = ["${video}"];
        "video/mp4" = ["${video}"];
        "audio/mpeg" = ["${video}"];
        "application/json" = ["${text}"];
        "application/yaml" = ["${text}"];
        "application/pdf" = ["${pdf}"];
        "x-scheme-handler/magnet" = ["${torrent}"];
        "x-scheme-handler/http" = ["${browser}"];
        "x-scheme-handler/https" = ["${browser}"];
        "text/html" = ["${text}"];
      };
      defaultApplications = {
        "image/png" = ["${image}"];
        "image/jpeg" = ["${image}"];
        "image/jpg" = ["${image}"];
        "image/tiff" = ["${image}"];
        "image/svg+xml" = ["${image}"];
        "video/x-matroska" = ["${video}"];
        "video/mp4" = ["${video}"];
        "audio/mpeg" = ["${video}"];
        "application/json" = ["${text}"];
        "application/yaml" = ["${text}"];
        "application/pdf" = ["${pdf}"];
        "x-scheme-handler/magnet" = ["${torrent}"];
        "x-scheme-handler/http" = ["${browser}"];
        "x-scheme-handler/https" = ["${browser}"];
        "text/html" = ["${text}"];
      };
    };
    userDirs = {
      enable = true;
      createDirectories = true;

      # These are useless to me
      desktop = null;
      documents = null;
      publicShare = null;
      templates = null;
      pictures = null;
      music = null;

      download = "${homeDir}/Scaricati";
      videos = "${homeDir}/Video";
    };
  };
}
