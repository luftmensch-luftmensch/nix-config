{pkgs}: let
  #   current_date = "${pkgs.coreutils}/bin/date '+%a %d %B %Y'";
  #   today = "${pkgs.coreutils}/bin/date '+%-d'";
  #   body =
  _date = "${pkgs.coreutils}/bin/date";
  _tail = "${pkgs.coreutils}/bin/tail";
  _cal = "${pkgs.busybox}/bin/cal";
  _sed = "${pkgs.gnused}/bin/sed";
in
  pkgs.writeShellScriptBin "poly-cal" ''
    current_date=$(${_date} '+%a %d %B %Y')
    today="$(${_date} '+%-d')"
    body=$(${_cal} | ${_tail} -n7 | ${_sed} -z "s|$today|<u><b>$today</b></u>|1")
    foot="\n<i>      ~ calendar</i> boop "

    ${pkgs.libnotify}/bin/notify-send  -t 1000 -u low "$current_date" "$body" "$foot"
  ''
