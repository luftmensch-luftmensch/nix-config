{ lib, pkgs, ... }:
let
  _acpi = "${lib.getExe pkgs.acpi} -b";
  _notify = "${lib.getExe pkgs.libnotify} -t 1000 -u critical";
in
pkgs.writeShellScriptBin "bss" ''
  function check_battery() {
    local capacity
    local status
    capacity="$(${_acpi} | awk '!/(rate information unavailable)/' | awk NR==1 | grep -io "[0-9]*%" | sed 's/%//g' )"
    status="$(${_acpi} | awk 'NR==1 {print $3}' | sed 's/,//g')"

    if [ "$capacity" -le 20 ] && [ "$capacity" -ge 11 ] && [ "$status" == "Discharging" ]; then
        ${_notify} -i battery_low "$capacity %: See you, space cowboy..."
    elif [ "$capacity" -le 10 ] && [ "$status" == "Discharging" ]; then
        ${_notify} -i battery_caution "Running on low battery!"
    elif [ "$capacity" -eq 100 ] && [ "$status" == "Full" ]; then
         ${_notify} -i battery "Fully recharged" "Please remove the power supply"
    fi
  }

  while true; do
    check_battery
    sleep 5m
  done
''
