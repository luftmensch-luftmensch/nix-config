{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.wayland;
in {
  options.valentino.modules.wayland = {
    enable = mkEnableOption "wayland configuration management for user";
    wm = mkOption {
      description = "An option to choose the window manager [wayland] configuration to enable";
      default = null;
      type = with types; nullOr (listOf (enum ["sway" "hyprland"]));
      example = "sway";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      bemenu
      wl-clipboard
      wl-mirror
      wf-recorder
      gsettings-desktop-schemas

      grim
      slurp
      sway-contrib.grimshot
      # libinput-gestures

      swayidle
      swaylock-effects
      wob
      swaynotificationcenter

      wtype
      slurp
      swaybg
      cliphist
    ];

    home.sessionVariables = {
      # export QT_QPA_PLATFORMTHEME=qt5ct

      GDK_BACKEND = "wayland,x11";
      XDG_SESSION_TYPE = "wayland";
      SDL_VIDEODRIVER = "wayland";
      # GTK_USE_PORTAL = "1";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      MOZ_ENABLE_WAYLAND = "1";
      MOZ_DBUS_REMOTE = "1";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      NIXOS_OZONE_WL = "1";
      # WLR_RENDERER_ALLOW_SOFTWARE = "1";
    };
  };
}
