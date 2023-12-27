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
  dbus-sway-environment = pkgs.writeTextFile {
    name = "dbus-sway-environment";
    destination = "/bin/dbus-sway-environment";
    executable = true;

    text = ''
      dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
      systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
      systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
    '';
  };

  configure-gtk = pkgs.writeTextFile {
    name = "configure-gtk";
    destination = "/bin/configure-gtk";
    executable = true;
    text = let
      schema = pkgs.gsettings-desktop-schemas;
      datadir = "${schema}/share/gsettings-schemas/${schema.name}";
    in ''
      export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
      gnome_schema=org.gnome.desktop.interface
      wm_schema=org.gnome.desktop.wm.preferences
      gsettings set $gnome_schema gtk-theme 'Adwaita-dark'
      gsettings set $gnome_schema document-font-name "Sarasa Mono Slab SC 13"
      gsettings set $gnome_schema font-name "Sarasa Mono Slab SC 13"
      gsettings set $gnome_schema monospace-font-name "Sarasa Mono Slab SC 13"
      gsettings set $wm_schema titlebar-font "Sarasa Mono Slab SC 13"
			gsettings set $gnome_schema color-scheme 'prefer-dark'
    '';
  };

  # chrome://flags/#enable-webrtc-pipewire-capturer (Enable it to share entire screen)
  chromium_on_wayland = with pkgs; (chromium.override {
    commandLineArgs = "--enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=WebRTCPipeWireCapturer --enable-usermedia-screen-capturing";
  });
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
          bemenu                    # Dmenu for wayland
          brightnessctl             # Manage brightness

          cliphist                  # Wayland clipboard manager
          configure-gtk             # Wrapper around gsettings
          
          wl-clipboard              # Command-line copy/paste utilities for Wayland
          wl-mirror                 # A simple Wayland output mirror client
                                    # Example usage: `wl-mirror -S <output>` (To get the output name use slurp -b \#00000000 -B \#00000000 -c \#859900 -w 4 -f %o -or 2>/dev/null)

          wf-recorder               # Utility program for screen recording of wlroots-based compositors

          foot                      # Wayland native terminal
          glib                      # GTK Theme & Font
          gsettings-desktop-schemas # Collections of GSettings schemas

          grim                      # Grab images from a Wayland compositor
          slurp                     # Select a region in a Wayland compositor
          sway-contrib.grimshot     # A helper for screenshots within sway (grimshot --notify copy area)
          libinput-gestures         # Gesture support under wayland

          swayidle                  # Idle management daemon for Wayland
          swaylock-effects          # A more customizable `swaylock`
          wob                       # A lightweight overlay volume/backlight/progress/anything bar for Wayland
          dbus-sway-environment     # Bash script to let dbus know about important env variables and propogate them to relevent services
          chromium_on_wayland
					waybar # Highly customizable Wayland bar for Sway and Wlroots based compositors
					# swaynotificationcenter # Simple notification daemon with a GUI built for Sway
        ];

        #export SUDO_ASKPASS="${pkgs.ksshaskpass}/bin/ksshaskpass"
        #export SSH_ASKPASS="${pkgs.ksshaskpass}/bin/ksshaskpass"
        #export GDK_BACKEND=wayland
        #export CLUTTER_BACKEND=wayland

        # some nixpkgs modules have wrapers that force electron apps to use wayland
        #export NIXOS_OZONE_WL = "1";

        # Fixing java apps (especially intellij) -> _JAVA_AWT_WM_NONREPARENTING = "1";

        extraSessionCommands = ''
          export MOZ_ENABLE_WAYLAND=1
          export MOZ_DBUS_REMOTE=1
          export SDL_VIDEODRIVER=wayland
          export QT_QPA_PLATFORM=wayland
          export QT_QPA_PLATFORMTHEME=qt5ct
          export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
          export _JAVA_AWT_WM_NONREPARENTING=1
          export XDG_SESSION_TYPE=wayland
          export XDG_CURRENT_DESKTOP=sway
          export GTK_THEME=Adwaita:dark
        '';
      };
    };

    # In addition to logind suspend on lid switch enable a service to lock the screen before suspend
    systemd.services."screen-locker" = {
      enable = true;
      description = "Lock the screen before suspend.";
      before = ["suspend.target"];
      wantedBy = ["suspend.target"];
      serviceConfig = {
        Type = "simple";
        Environment = "DISPLAY=:1 WAYLAND_DISPLAY=wayland-1 XDG_RUNTIME_DIR=/run/user/1000";
        User = "valentino";
        ExecStart = "${pkgs.swaylock-effects}/bin/swaylock --clock --indicator --screenshots --fade-in 1 --ignore-empty-password --ring-color 0F0F0F --indicator-thickness 7 --indicator-radius 100 --line-color 00000000 --inside-color 00000088 --separator-color 00000000 --effect-vignette 0.2:0.5 --effect-scale 0.4 --effect-blur 7x5 --key-hl-color 880033";
      };
    };
  };
}
