{
  lib,
  menu_opts,
  pkgs,
}:
let
  _clip = "${pkgs.clipcat}/bin/clipcatctl";
  _notify = "${lib.getExe pkgs.libnotify} -t 500";
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

  select_item(){
      if [[ "$(${_clip} list | wc -l)" -eq 0 ]]; then
        ${_notify} -u normal "No items stored yet!" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i ~/Dropbox/icons/checked.png
      else
        selected="$(${_clip} list | ${_menu} -p '▶ Copy to Clipboard: ' -W 0.5 | cut -d: -f1)"
        [[ -n "$selected" ]] && ${_clip} promote "$selected"
      fi
  }

  delete_item(){
      if [[ "$(${_clip} list | wc -l)" -eq 0 ]]; then
        ${_notify} -u normal "No items stored yet!" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i ~/Dropbox/icons/checked.png
      else
        selected="$(${_clip} list | ${_menu} -p '▶ Delete item from Clipboard: '-W 0.5 | cut -d: -f1)"
        [[ -n "$selected" ]] && ${_clip} remove "$selected"
      fi
  }

  delete_all_items(){
      if ${_clip} clear; then
        ${_notify} -u normal "Database deleted" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i ~/Dropbox/icons/checked.png
      else
        ${_notify} -u normal "Failed to clear the database" -a CLIPBOARD_MANAGER -r ''${notification_id} -p -i ~/Dropbox/icons/trash.svg
      fi
  }

  chosen=$(printf '%s\n' "''${menu[@]}" | ${_menu} -p '▶ Choose an option: ' -W 0.2)
  case $chosen in
    "Select item") select_item ;;
    "Delete item") delete_item ;;
    "Delete all items") delete_all_items ;;
  esac
''
