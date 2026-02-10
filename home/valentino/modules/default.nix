{
  valentino = {
    "global"                    = import ./global;

    "apps/rofi"                 = import ./apps/rofi;
    "apps/dunst"                = import ./apps/dunst.nix;
    "apps/outlook"              = import ./apps/outlook.nix;
    "apps/playerctl"            = import ./apps/playerctl.nix;
    "apps/discord"              = import ./apps/discord.nix;
    "apps/teams"                = import ./apps/teams.nix;
    "apps/thunderbird"          = import ./apps/thunderbird.nix;
    "apps/vnc"                  = import ./apps/vnc.nix;

    "browsers/firefox"          = import ./browsers/firefox.nix;
    "browsers/chromium"         = import ./browsers/chromium.nix;

    "credentials/ssh"           = import ./credentials/ssh;
    "credentials/1password"     = import ./credentials/1password.nix;
    "credentials/gpg"           = import ./credentials/gpg.nix;
    "credentials/mail"          = import ./credentials/mail.nix;
    "credentials/proton"        = import ./credentials/proton.nix;
    "credentials/sops"          = import ./credentials/sops.nix;

    "dev/android"               = import ./dev/android.nix;
    "dev/cc"                    = import ./dev/cc.nix;
    "dev/generics"              = import ./dev/generics.nix;
    "dev/java"                  = import ./dev/java.nix;
    "dev/js"                    = import ./dev/js.nix;
    "dev/nix"                   = import ./dev/nix-lang.nix;
    "dev/python"                = import ./dev/python.nix;
    "dev/rust"                  = import ./dev/rust.nix;
    "dev/tex"                   = import ./dev/tex.nix;
    "dev/tooling"               = import ./dev/tooling.nix;

    "editors/emacs"             = import ./editors/emacs;
    "editors/android-studio"    = import ./editors/android-studio.nix;
    "editors/intellij"          = import ./editors/intellij.nix;
    "editors/neovim"            = import ./editors/neovim.nix;
    "editors/vscode"            = import ./editors/vscode;

    "graphical"                 = import ./graphical;

    "media/documents"           = import ./media/documents.nix;
    "media/editing"             = import ./media/editing.nix;
    "media/files"               = import ./media/files.nix;
    "media/images"              = import ./media/images.nix;
    "media/music"               = import ./media/music.nix;
    "media/reading"             = import ./media/reading.nix;
    "media/videos"              = import ./media/videos.nix;

    "services/battery"          = import ./services/battery.nix;

    "shell/bash"                = import ./shell/bash.nix;
    "shell/direnv"              = import ./shell/direnv.nix;
    "shell/extensions"          = import ./shell/extensions.nix;
    "shell/fish"                = import ./shell/fish;
    "shell/git"                 = import ./shell/git.nix;
    "shell/nix-index"           = import ./shell/nix-index.nix;

    "term/alacritty"            = import ./term/alacritty.nix;
    "term/foot"                 = import ./term/foot.nix;
    "term/kitty"                = import ./term/kitty.nix;

    "wayland"                   = import ./wayland;
    "wayland/sway"              = import ./wayland/sway;
    "wayland/swaync"            = import ./wayland/swaync;
    "wayland/waybar"            = import ./wayland/waybar;
    "wayland/locker"            = import ./wayland/locker.nix;
    "wayland/random-background" = import ./wayland/random-background.nix;

    "xorg"                      = import ./xorg;
    "xorg/i3"                   = import ./xorg/i3;
    "xorg/clipboard"            = import ./xorg/clipboard.nix;
    "xorg/locker"               = import ./xorg/locker.nix;
    "xorg/picom"                = import ./xorg/picom.nix;
    "xorg/polybar"              = import ./xorg/polybar;
    "xorg/xob"                  = import ./xorg/xob.nix;

    "themes"                    = import ./themes;
    "themes/fonts"              = import ./themes/fonts.nix;
  };
}
