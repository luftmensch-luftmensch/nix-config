{pkgs, ...}: let
  bctl = "${pkgs.bluez}/bin/bluetoothctl";
  sctl = "${pkgs.systemd}/bin/systemctl";
  _grep = "${pkgs.gnugrep}/bin/grep";
  _cut = "${pkgs.coreutils}/bin/cut";
in
  pkgs.writeShellScriptBin "bluetooth-ctl" ''
    # https://www.jvt.me/posts/2021/12/10/bluetooth-percent-linux/

    bluetooth_print() {
      ${bctl} | while read -r; do
        if [ "$(${sctl} is-active "bluetooth.service")" = "active" ]; then
          printf ' '
          devices_paired=$(${bctl} devices Paired | ${_grep} Device | ${_cut} -d ' ' -f 2)
          counter=0

          echo "$devices_paired" | while read -r line; do
            device_info=$(${bctl} info "$line")

            if echo "$device_info" | ${_grep} -q "Connected: yes"; then
              device_alias=$(echo "$device_info" | ${_grep} "Alias" | ${_cut} -d ' ' -f 2-)

              if [ $counter -gt 0 ]; then
                printf "+ %s" "$device_alias"
              else
                printf "%s " "$device_alias"
              fi

              counter=$((counter + 1))
            fi
          done

          printf '\n'
        else
          echo "#2"
        fi
      done
    }

    bluetooth_toggle() {
      if ${bctl} show | ${_grep} -q "Powered: no"; then
        ${bctl} power on >> /dev/null
        ${pkgs.coreutils}/bin/sleep 1

        devices_paired=$(${bctl} paired-devices | ${_grep} Device | ${_cut} -d ' ' -f 2)
        echo "$devices_paired" | while read -r line; do
          ${bctl} connect "$line" >> /dev/null
        done
      else
        devices_paired=$(${bctl} paired-devices | ${_grep} Device | ${_cut} -d ' ' -f 2)
        echo "$devices_paired" | while read -r line; do
          ${bctl} disconnect "$line" >> /dev/null
        done

        ${bctl} power off >> /dev/null
      fi
    }

    case "$1" in
      --toggle) bluetooth_toggle ;;
      *) bluetooth_print ;;
    esac
  ''
