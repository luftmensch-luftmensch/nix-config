pkgs: let
  _slurp = "${pkgs.slurp}/bin/slurp";
  _notify = "${pkgs.libnotify}/bin/notify-send -t 800 -u low";
  _wl-mirror = "${pkgs.wl-mirror}/bin/wl-mirror";
  _fzf = "${pkgs.fzf}/bin/fzf --reverse --height=50%  -i --pointer=\"▶\"   --info=inline --border=rounded --cycle";
in
  pkgs.writeShellScriptBin "screen-mirroring" ''
    set -eou pipefail
    # Menu list
    declare -a menu=(
        "  Mirror entire screen"
        "  Mirror portion"
        "  Info"
        "  Exit"
    )

    usage() {
        echo -e "usage: $(basename "$0") <command> [argument]"
        echo
        echo "start wl-mirror and control the mirrored output or region in a convenient way"
        echo
        echo "commands:"
        echo "  Info                show this help"
        echo "  mirror [entire screen]     start wl-mirror on output [output] (default asks via slurp)"
        echo "  mirror [portion]     start wl-mirror on output [output] (default asks via slurp)"
        exit 0
    }

    # Cheating way to disown wl-mirror in order to reuse the shell
    screen_mirroring_pipe=/tmp/screen_mirroring_pipe
    chosen_option=$(printf '%s\n' "''${menu[@]}" | ${_fzf})


    mirror-screen(){
        # We define it local in order to execute it only when needed
        local selected_output
        selected_output="$(${_slurp} -b \#00000000 -B \#00000000 -c \#859900 -w 4 -f %o -or)"

        # Cheating way to disown wl-mirror in order to reuse the shell
        [ -p "$screen_mirroring_pipe" ] || mkfifo -m 0600 "$screen_mirroring_pipe" || exit 1
        ${_notify} " Starting screen mirroring of < ''${selected_output}>"
        tail "$screen_mirroring_pipe" | ${_wl-mirror} -S "''${selected_output}" & disown
    }

    mirror-portion(){
        # We define it local in order to execute it only when needed
        local selected_region
        selected_region=$(${_slurp} -b \#00000000 -c \#859900 -w 2 2>/dev/null)


        # Cheating way to disown wl-mirror in order to reuse the shell
        [ -p "$screen_mirroring_pipe" ] || mkfifo -m 0600 "$screen_mirroring_pipe" || exit 1
        ${_notify} " Starting screen mirroring of portion "
        tail "$screen_mirroring_pipe" | ${_wl-mirror} -r "''${selected_region}" & disown
    }

    case "$chosen_option" in
        "  Mirror entire screen") mirror-screen ;;
        "  Mirror portion") mirror-portion ;;
        "  Info") usage ;;
        " Exit") exit 0 ;;
    esac
  ''
