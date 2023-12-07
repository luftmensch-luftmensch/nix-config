{
  "core/cachix"                 = import ./core/cachix.nix;
  "core/environment"            = import ./core/environment.nix;
  "core/impermanence"           = import ./core/impermanence.nix;
  "core/locale"                 = import ./core/locale.nix;
  "core/network"                = import ./core/network.nix;
  "core/quiet-boot"             = import ./core/quiet-boot.nix;
  "core/security"               = import ./core/security.nix;
  "core/settings"               = import ./core/settings.nix;
  "core/user"                   = import ./core/user.nix;

  "credentials/ssh"             = import ./credentials/ssh.nix;
  "credentials/gpg"             = import ./credentials/gpg.nix;

  "dev/adb"                     = import ./dev/adb.nix;
  "dev/docker"                  = import ./dev/docker.nix;
  "dev/kubernetes"              = import ./dev/kubernetes.nix;
  "dev/manpages"                = import ./dev/manpages.nix;
  "dev/virtualisation"          = import ./dev/virtualisation.nix;

  "editor/emacs"                = import ./editor/emacs.nix;
  "editor/ide"                  = import ./editor/ide.nix;
  "editor/neovim"               = import ./editor/neovim.nix;

  "graphical/common"            = import ./graphical/common.nix;
  "graphical/portals"           = import ./graphical/portals.nix;
  "graphical/sddm"              = import ./graphical/sddm.nix;
  "graphical/xorg"              = import ./graphical/xorg.nix;
  "graphical/wayland"           = import ./graphical/wayland.nix;

  "hardware/audio"              = import ./hardware/audio.nix;
  "hardware/bluetooth"          = import ./hardware/bluetooth.nix;

  "packages/base"               = import ./packages/base.nix;
  "packages/dmenu-with-patches" = import ./packages/dmenu-with-patches.nix;
  "packages/gaming"             = import ./packages/gaming.nix;
  "packages/miscellaneous"      = import ./packages/miscellaneous.nix;
  "packages/programming"        = import ./packages/programming.nix;
  "packages/ricing"             = import ./packages/ricing.nix;
  "packages/secrets"            = import ./packages/secrets.nix;
  "packages/unstable"           = import ./packages/unstable.nix;
  "packages/utilities"          = import ./packages/utilities.nix;
  "packages/xorg-related"       = import ./packages/xorg-related.nix;

  "services/battery"            = import ./services/battery.nix;
  "services/fprintd"            = import ./services/fprintd.nix;
  "services/logind"             = import ./services/logind.nix;
  "services/printing"           = import ./services/printing.nix;
  "services/rules"              = import ./services/rules.nix;
  "services/syncthing"          = import ./services/syncthing.nix;
  "services/touchpad"           = import ./services/touchpad.nix;

  "vpn/openvpn"                 = import ./vpn/openvpn.nix;
  "vpn/wireguard"               = import ./vpn/wireguard.nix;

  "shell/bash"                  = import ./shell/bash.nix;
  "shell/fish"                  = import ./shell/fish.nix;
}
