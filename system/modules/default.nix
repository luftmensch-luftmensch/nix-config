{
  "core/cachix"        = import ./core/cachix.nix;
  "core/environment"   = import ./core/environment.nix;
  "core/hardening"     = import ./core/hardening.nix;
  "core/impermanence"  = import ./core/impermanence.nix;
  "core/locale"        = import ./core/locale.nix;
  "core/luks"          = import ./core/luks.nix;
  "core/network"       = import ./core/network.nix;
  "core/quiet-boot"    = import ./core/quiet-boot.nix;
  "core/settings"      = import ./core/settings.nix;
  "core/slim"          = import ./core/slim.nix;
  "core/user"          = import ./core/user.nix;

  "credentials/gpg"    = import ./credentials/gpg.nix;
  "credentials/sops"   = import ./credentials/sops.nix;
  "credentials/ssh"    = import ./credentials/ssh.nix;

  "dev/adb"            = import ./dev/adb.nix;
  "dev/docker"         = import ./dev/docker.nix;
  "dev/keyboard"       = import ./dev/keyboard.nix;
  "dev/manpages"       = import ./dev/manpages.nix;
  "dev/virtualisation" = import ./dev/virtualisation.nix;

  "graphical/common"   = import ./graphical/common.nix;
  "graphical/portals"  = import ./graphical/portals.nix;
  "graphical/sddm"     = import ./graphical/sddm.nix;
  "graphical/xorg"     = import ./graphical/xorg.nix;
  "graphical/wayland"  = import ./graphical/wayland.nix;

  "hardware/audio"     = import ./hardware/audio.nix;
  "hardware/bluetooth" = import ./hardware/bluetooth.nix;
  "hardware/video"     = import ./hardware/video.nix;

  "services/battery"   = import ./services/battery.nix;
  "services/fprintd"   = import ./services/fprintd.nix;
  "services/logind"    = import ./services/logind.nix;
  "services/printing"  = import ./services/printing.nix;
  "services/rules"     = import ./services/rules.nix;
  "services/syncthing" = import ./services/syncthing.nix;
  "services/touchpad"  = import ./services/touchpad.nix;

  "vpn/openconnect"    = import ./vpn/openconnect.nix;
  "vpn/openvpn"        = import ./vpn/openvpn.nix;
  "vpn/wireguard"      = import ./vpn/wireguard.nix;

  "shell/bash"         = import ./shell/bash.nix;
}
