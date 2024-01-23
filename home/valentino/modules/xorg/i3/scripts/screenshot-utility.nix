{
  theme,
	colors,
  pkgs,
}: let
  _menu = "${pkgs.bemenu}/bin/bemenu -i --fn '${theme.font.regular.family} ${(toString theme.font.regular.size)}' --nb '#${colors.base00}'  --tb '#${colors.base01}' --hb '#${colors.base02}' --tf '#${colors.base0D}' --hf '#${colors.base0D}' -p '▶ Type: '";
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
