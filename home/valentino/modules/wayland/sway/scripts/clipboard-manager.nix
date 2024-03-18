{
  menu_opts,
  pkgs,
}: let
  _clip = "${pkgs.cliphist}/bin/cliphist";
  _notify = "${pkgs.libnotify}/bin/notify-send -t 500";
  _wlcp = "${pkgs.wl-clipboard}/bin/wl-copy";
  _menu = "${pkgs.bemenu}/bin/bemenu ${menu_opts} -c -i -l 5";
in
  pkgs.writeShellScriptBin "cms" ''
    set -e

    declare -a menu=(
      "Select item"
      "Delete item"
      "Delete all items"
    )

    notification_id=1

    cliphist_db_location="$HOME/.cache/cliphist/db"

    chosen=$(printf '%s\n' "''${menu[@]}" | ${_menu} -p '▶ Choose an option: ' -W 0.2)

    select_item(){
        if [[ "$(${_clip} list | wc -l)" -eq 0 ]]; then
          ${_notify} -u normal "No items stored yet!" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i user-trash
        else
          ${_clip} list | ${_menu} -p '▶ Copy to Clipboard: ' -W 0.5 | ${_clip} decode | ${_wlcp}
        fi
    }

    delete_item(){
        if [[ "$(${_clip} list | wc -l)" -eq 0 ]]; then
          ${_notify} -u critical "No items stored yet!" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i user-trash
        else
          ${_clip} list | ${_menu} -p '▶ Copy to Clipboard: ' -W 0.5 | ${_clip} delete
        fi

    }

    delete_all_items(){
        [ -f "$cliphist_db_location" ] && rm "$cliphist_db_location"
        ${_notify} -u low "Database deleted" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i ~/Dropbox/icons/checked.png
    }


    case $chosen in
      "Select item") select_item ;;
      "Delete item") delete_item ;;
      "Delete all items") delete_all_items ;;
    esac
  ''
