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

  media = with pkgs; [
    pavucontrol
    brightnessctl
    ffmpeg-full
    # playerctl
    exiftool
    imagemagick
  ];

  # Why is my laptop so hot? Oh yeah I'm compiling 20 programs while running two games
  monitoring = with pkgs; [
    btop
    s-tui
    speedtest-cli
  ];

  utilities = with pkgs; [
    bind       # Domain name server
    lm_sensors # Tools for reading hardware sensors
    lshw       # Info on the HW configuration of the machine
    lsof       # List open files
    nmap       # Network exploration tool and security / port scanner
    procs      # Modern replacement for ps
    ps_mem     # Usage: sudo ps_mem -p $(pgrep -d, -u $USER) (Why is Emacs using so much RAM?)
    traceroute # Print the route packets trace to network host
    unrar      # Utility for RAR archives
    unzip      # list, test and extract compressed files in a ZIP archive
    usbutils   # Tools for working with USB devices, such as lsusb
    zip        # package and compress (archive) files
  ];

  desktop = with pkgs; [
    transmission-gtk
    cinnamon.nemo-with-extensions
    gnome.gnome-disk-utility
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
      ++ fonts
      ++ monitoring
      ++ utilities
      ++ media;

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
