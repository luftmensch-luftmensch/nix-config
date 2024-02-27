{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  inherit (config.valentino.modules) wayland xorg;

  # Creative XOR operator :D
  cfgExclusive = (xorg.enable || wayland.enable) && (!(xorg.enable && wayland.enable));

  # To get list of the font installed: (fc-list : family)
  fonts = with pkgs;
    [
      apple-fonts
      font-awesome
      fira-code
      monoid
      (nerdfonts.override {fonts = ["Iosevka"];})
      iosevka-comfy.comfy
      source-code-pro
      sarasa-gothic
      victor-mono
      cantarell-fonts
      scientifica

      # Micro$oft
      corefonts

      noto-fonts
      noto-fonts-emoji
      noto-fonts-cjk-sans

      material-design-icons
      ibm-plex
    ]
    ++ [icomoon-feather-icons phosphor-icons];

  media = with pkgs; [
    pavucontrol
    brightnessctl
    ffmpeg-full
    exiftool
    imagemagick
  ];

  # Why is my laptop so hot? Oh yeah I'm compiling 20 programs while running two games
  monitoring = with pkgs; [
    btop
    s-tui
    speedtest-go
  ];

  utilities = with pkgs; [
    bind # Domain name server
    lm_sensors # Tools for reading hardware sensors
    lshw # Info on the HW configuration of the machine
    lsof # List open files
    nmap # Network exploration tool and security / port scanner
    procs # Modern replacement for ps
    ps_mem # Usage: sudo ps_mem -p $(pgrep -d, -u $USER) (Why is Emacs using so much RAM?)
    traceroute # Print the route packets trace to network host
    unrar # Utility for RAR archives
    unzip # list, test and extract compressed files in a ZIP archive
    usbutils # Tools for working with USB devices, such as lsusb
    zip # package and compress (archive) files
  ];

  desktop = with pkgs; [
    transmission-gtk
    cinnamon.nemo-with-extensions
    gnome.gnome-disk-utility
    networkmanagerapplet
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
