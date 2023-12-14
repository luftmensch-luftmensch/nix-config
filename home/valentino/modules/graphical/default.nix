{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfgXorg = config.valentino.modules.xorg;
  cfgWayland = config.valentino.modules.wayland;

  # Creative XOR operator :D
  cfgExclusive =
    (cfgXorg.enable || cfgWayland.enable)
    && (!(cfgXorg.enable && cfgWayland.enable));

  # To get list of the font installed: (fc-list : family)
  # Valid font names https://github.com/NixOS/nixpkgs/blob/6ba3207643fd27ffa25a172911e3d6825814d155/pkgs/data/fonts/nerdfonts/shas.nix
  fonts = with pkgs; [
    font-awesome
    fira-code
    monoid # https://larsenwork.com/monoid/

    (nerdfonts.override {fonts = ["Iosevka"];})
    source-code-pro
    sarasa-gothic # A CJK programming font based on Iosevka and Source Han Sans
    victor-mono
    cantarell-fonts
    scientifica   

    # Micro$oft
    corefonts
    # Noto
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk-sans
    # Icons
    material-design-icons
    # Fira
    # fira
    # IBM Plex Mono - Waybar
    ibm-plex
  ];

  social = with pkgs; []; # tdesktop

  media = with pkgs; [
    pavucontrol
    brightnessctl
    ffmpeg-full
    playerctl
    exiftool
    imagemagick
  ];

  # Why is my laptop so hot? Oh yeah I'm compiling 20 programs while running two games
  monitoring = with pkgs; [
    btop
    s-tui
    speedtest-cli
  ];

  # files = let
  #   thunarPlugins = with pkgs; [
  #     xfce.thunar-volman
  #     xfce.thunar-archive-plugin
  #     xfce.thunar-media-tags-plugin
  #   ];
  # in
  #   with pkgs; [

  #   ];

  desktop = with pkgs; [
    transmission-gtk
    cinnamon.nemo
  ];
in {
  config = {
    assertions = [
      {
        assertion = cfgExclusive;
        message = "Can't enable customization for both Xorg and Wayland.";
      }
    ];

    home.packages =
      desktop
      # ++ files
      ++ fonts
      ++ monitoring
      ++ media
      ++ social;

    fonts.fontconfig.enable = true;

    systemd.user.services = {
      polkit = {
        Unit = {
          Description = "polkit-gnome";
          Documentation = ["man:polkit(8)"];
          PartOf = ["graphical-session.target"];
        };
        Service = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          RestartSec = 3;
          Restart = "always";
        };
        Install = {WantedBy = ["graphical-session.target"];};
      };
    };
  };
}
