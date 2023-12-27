{
  theme,
  pkgs,
}: let
  # TODO: Do not hardcode the style
  _menu = "${pkgs.bemenu}/bin/bemenu -i --fn '${theme.font.regular.family}:size=${(toString theme.font.regular.size)}' --tb '#3B4252' --nb '#0F0F0F' --nf '#c5c8c6' --sb '#3B4252' --sf '#c5c8c6' --tf '#FFFFFF' --hf '#FFFFFF' --hb '#3B4252' -p '▶ Type: '";
  _scrot = "${pkgs.scrot}/bin/scrot";
	_notify = "${pkgs.libnotify}/bin/notify-send -u low -t 700";
in
  pkgs.writeShellScriptBin "sus" ''

    declare -a menu=(
      "Fullscreen"
      "section"
      "window"
    )

    chosen=$(printf '%s\n' "''${menu[@]}" | ${_menu})

    filename="$(date +%d-%m-%Y-%H:%M:%S).png"

    case $chosen in
        Fullscreen) ${_scrot} -d 1 '%d-%m-%Y-@%H:%M:%S.png' && ${_notify} 'Fullscreen screenshot saved' ;;
        section) ${_scrot} -s --freeze '%d-%m-%Y-@%H:%M:%S.png' && ${_notify} 'Screenshot saved' ;;
        window) ${_scrot} -u '%d-%m-%Y-@%H:%M:%S.png' && ${_notify} 'screenshot saved' ;;
    esac
  ''
