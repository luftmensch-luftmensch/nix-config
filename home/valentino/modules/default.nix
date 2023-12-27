{
  valentino = {
    "global"                 = import ./global;

    "apps/polybar"           = import ./apps/polybar;
    "apps/rofi"              = import ./apps/rofi;
    "apps/api_tools"         = import ./apps/api_tools.nix;
    "apps/dunst"             = import ./apps/dunst.nix;
    "apps/discord"           = import ./apps/discord.nix;
    "apps/teams"             = import ./apps/teams.nix;

    "browsers/firefox"       = import ./browsers/firefox.nix;
    "browsers/chromium"      = import ./browsers/chromium.nix;

    "credentials/ssh"        = import ./credentials/ssh;
    "credentials/sops"       = import ./credentials/sops.nix;
    "credentials/bitwarden"  = import ./credentials/bitwarden.nix;
    "credentials/gpg"        = import ./credentials/gpg.nix;
    "credentials/mail"       = import ./credentials/mail.nix;

    "dev/cc"                 = import ./dev/cc.nix;
    "dev/generics"           = import ./dev/generics.nix;
    "dev/java"               = import ./dev/java.nix;
    "dev/js"                 = import ./dev/js.nix;
    "dev/nix"                = import ./dev/nix-lang.nix;
    "dev/python"             = import ./dev/python.nix;
    "dev/rust"               = import ./dev/rust.nix;
    "dev/tex"                = import ./dev/tex.nix;

    "editors/android-studio" = import ./editors/android-studio.nix;
    "editors/intellij"       = import ./editors/intellij.nix;
    "editors/vscode"         = import ./editors/vscode;

    "gaming/emulators"       = import ./gaming/emulators.nix;
    "gaming/steam"           = import ./gaming/steam.nix;

    "graphical"              = import ./graphical;

    "media/documents"        = import ./media/documents.nix;
    "media/editing"          = import ./media/editing.nix;
    "media/files"            = import ./media/files.nix;
    "media/images"           = import ./media/images.nix;
    "media/music"            = import ./media/music.nix;
    "media/videos"           = import ./media/videos.nix;

    "shell/bash"             = import ./shell/bash.nix;
    "shell/zsh"              = import ./shell/zsh.nix;
    "shell/direnv"           = import ./shell/direnv.nix;
    "shell/extensions"       = import ./shell/extensions.nix;
    "shell/fish"             = import ./shell/fish;
    "shell/git"              = import ./shell/git.nix;
    "shell/starship"         = import ./shell/starship.nix;
    "shell/tmux"             = import ./shell/tmux.nix;

    "term/alacritty"         = import ./term/alacritty.nix;
    "term/foot"              = import ./term/foot.nix;

    "wayland"                = import ./wayland;
    "wayland/sway"           = import ./wayland/sway;
    "wayland/hyprland"       = import ./wayland/hyprland.nix;
    "wayland/locker"         = import ./wayland/locker.nix;
    "wayland/waybar"         = import ./wayland/waybar;

    "xorg"                   = import ./xorg;
    "xorg/i3"                = import ./xorg/i3;
    "xorg/dmenu"             = import ./xorg/dmenu.nix;
    "xorg/locker"            = import ./xorg/locker.nix;
    "xorg/picom"             = import ./xorg/picom.nix;
    "xorg/xob"               = import ./xorg/xob.nix;

    "themes"                 = import ./themes;
    "themes/fonts"           = import ./themes/fonts.nix;
  };
}
