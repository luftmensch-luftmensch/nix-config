{
  theme,
  pkgs,
}: let
  _clip = "${pkgs.cliphist}/bin/cliphist";
  _notify = "${pkgs.libnotify}/bin/notify-send -t 500";
  _wlcp = "${pkgs.wl-clipboard}/bin/wl-copy";

  # TODO: Do not hardcode the style
  _menu = "${pkgs.bemenu}/bin/bemenu -c -i -W 0.5 -l 5 --fn '${theme.font.regular.font.family}:size=${(toString theme.font.regular.font.size)}' --tb '#3B4252' --nb '#0F0F0F' --nf '#c5c8c6' --sb '#3B4252' --sf '#c5c8c6' --tf '#FFFFFF' --hf '#FFFFFF' --hb '#3B4252' -p";
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

    print_error() {
        printf "\e[31m%b\e[0m" "$1"
        printf "\n"
    }

    chosen=$(printf '%s\n' "''${menu[@]}" | ${_menu} '▶ Choose an option: ')

    select_item(){
        if [[ "$(${_clip} list | wc -l)" -eq 0 ]]; then
          ${_notify} -u normal "No items stored yet!" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i ~/Dropbox/icons/empty_bin.png
        else
          ${_clip} list | ${_menu} '▶ Copy to Clipboard: ' | ${_clip} decode | ${_wlcp}
        fi
    }

    delete_item(){
        if [[ "$(${_clip} list | wc -l)" -eq 0 ]]; then
          ${_notify} -u critical "No items stored yet!" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i ~/Dropbox/icons/empty_bin.png
        else
          ${_clip} list | ${_menu} '▶ Copy to Clipboard: ' | ${_clip} delete
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
