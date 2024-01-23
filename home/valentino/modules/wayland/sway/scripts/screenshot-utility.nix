{
  menu_opts,
  pkgs,
}: let
  _menu = "${pkgs.bemenu}/bin/bemenu ${menu_opts} -p '▶ Type: '";
  _grim = "${pkgs.grim}/bin/grim";
  _swaymsg = "${pkgs.sway}/bin/swaymsg";
  _jq = "${pkgs.jq}/bin/jq";
  _wlcp = "${pkgs.wl-clipboard}/bin/wl-copy";
  _slurp = "${pkgs.slurp}/bin/slurp";
in
  pkgs.writeShellScriptBin "sus" ''

    declare -a menu=(
      "Fullscreen"
      "section"
      "window"
      "Screenshot+clipboard"
    )

    chosen=$(printf '%s\n' "''${menu[@]}" | ${_menu})

    filename="$(date +%d-%m-%Y-%H:%M:%S).png"

    case $chosen in
        Fullscreen) ${_grim} -o "$(${_swaymsg} -t get_outputs | ${_jq} -r '.[] | select(.focused) | .name')" "$filename" ;;
        section) ${_grim} -g "$(${_slurp})" "$filename" ;;
        window) ${_grim} -g "$(${_swaymsg} -t get_tree | ${_jq} -j '.. | select(.type?) | select(.focused).rect | "\(.x),\(.y) \(.width)x\(.height)"')"  "$filename" ;;
        "Screenshot+clipboard") ${_grim} -g "$(${_slurp} -d)" - | ${_wlcp} -t image/png #(screenshot+clipboard) ;;
    esac
  ''
