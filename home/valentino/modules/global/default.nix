{
  pkgs,
  config,
  lib,
  outputs,
  ...
}: {
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

  # home.file =
  #   lib.attrsets.concatMapAttrs
  #     (name: value: {
  #       ${name} = {
  #         target = "${config.xdg.userDirs.pictures}/walls/${name}.${value.ext}";
  #         source = value.src;
  #       };
  #     })
  #     outputs.wallpapers;

  xdg.userDirs = {
    enable = true;
    createDirectories = true;

    # These are useless to me
    desktop = null;
    publicShare = null;
    templates = null;
    pictures = null;
    # pictures = "${config.home.homeDirectory}/pics";

    documents = "${config.home.homeDirectory}/Documenti";
    download = "${config.home.homeDirectory}/Scaricati";
    music = "${config.home.homeDirectory}/Music";
    videos = "${config.home.homeDirectory}/Video";

    # extraConfig = {
    #   XDG_PROJECTS_DIR = "${config.home.homeDirectory}/projects";
    #   XDG_WORK_DIR = "${config.home.homeDirectory}/work";
    #   XDG_GAMES_DIR = "${config.home.homeDirectory}/games";
    #   XDG_MAILS_DIR = "${config.home.homeDirectory}/mails";
    # };
  };
}
