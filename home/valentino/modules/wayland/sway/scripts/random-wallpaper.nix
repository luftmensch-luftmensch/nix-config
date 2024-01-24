{
  wallpaper_path,
  pkgs,
  ...
}:
pkgs.writeShellScriptBin "rws" ''

    for pid in $(pgrep -f "$0"); do
        if [ "$pid" != $$ ]; then
            kill "$pid"
        fi
    done
    function set_wallpaper() {
      ${pkgs.sway}/bin/swaymsg -r -q output "*" bg '"$(find ${wallpaper_path} -type f | shuf -n1)"' fill
    }

  while true; do
    set_wallpaper
    sleep 900
  done
''
