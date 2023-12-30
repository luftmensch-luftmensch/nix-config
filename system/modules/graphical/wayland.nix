{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.wayland;
  # bash script to let dbus know about important env variables and propogate them to relevent services
  # run at the end of sway config
  # see https://github.com/emersion/xdg-desktop-portal-wlr/wiki/"It-doesn't-work"-Troubleshooting-Checklist
  # dbus-sway-environment = pkgs.writeTextFile {
  #   name = "dbus-sway-environment";
  #   destination = "/bin/dbus-sway-environment";
  #   executable = true;
  #   text = ''
  #     dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
  #     systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
  #     systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
  #   '';
  # };
in {
  options.system.modules.graphical.wayland = {
    enable = mkEnableOption "Wayland basic configuration and packages";
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager.defaultSession = "sway";
    programs = {
      sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = with pkgs; [
          # bemenu                    # Dmenu for wayland
          # brightnessctl             # Manage brightness

          cliphist # Wayland clipboard manager

          # wl-clipboard
          # wl-mirror

          # wf-recorder

          # foot
          glib
          gsettings-desktop-schemas

          grim
          slurp
          # sway-contrib.grimshot
          # libinput-gestures

          # swayidle
          # swaylock-effects
          # wob
          # dbus-sway-environment
          # waybar
          # swaynotificationcenter
        ];

        # extraSessionCommands = ''
        #   export MOZ_ENABLE_WAYLAND=1
        #   export MOZ_DBUS_REMOTE=1
        #   export SDL_VIDEODRIVER=wayland
        #   export QT_QPA_PLATFORM=wayland
        #   export QT_QPA_PLATFORMTHEME=qt5ct
        #   export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        #   export _JAVA_AWT_WM_NONREPARENTING=1
        #   export XDG_SESSION_TYPE=wayland
        #   export XDG_CURRENT_DESKTOP=sway
        #   export GTK_THEME=Adwaita:dark
        # '';
      };
    };

    # In addition to logind suspend on lid switch enable a service to lock the screen before suspend
    # systemd.services."screen-locker" = {
    #   enable = true;
    #   description = "Lock the screen before suspend.";
    #   before = ["suspend.target"];
    #   wantedBy = ["suspend.target"];
    #   serviceConfig = {
    #     Type = "simple";
    #     Environment = "DISPLAY=:1 WAYLAND_DISPLAY=wayland-1 XDG_RUNTIME_DIR=/run/user/1000";
    #     User = "valentino";
    #     ExecStart = "${pkgs.swaylock-effects}/bin/swaylock --clock --indicator --screenshots --fade-in 1 --ignore-empty-password --ring-color 0F0F0F --indicator-thickness 7 --indicator-radius 100 --line-color 00000000 --inside-color 00000088 --separator-color 00000000 --effect-vignette 0.2:0.5 --effect-scale 0.4 --effect-blur 7x5 --key-hl-color 880033";
    #   };
    # };
  };
}
