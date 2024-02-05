#!/usr/bin/env bash

# Configurations folder
themes_dir="$HOME/.config/rofi/themes"
theme="$themes_dir/powermenu"

uptime_command=$(uptime -p | sed -e 's/up //g')
rofi_command="rofi -theme $theme"

# Options
shutdown=" "
reboot=" "
lock=""
suspend=" "
logout=" "

logout() {
    if [[ "$DESKTOP_SESSION" =~ "qtile" ]]; then
        execute_with_confirm "qtile cmd-obj -o cmd -f shutdown"
    elif [[ "$DESKTOP_SESSION" =~ "i3" ]]; then
        execute_with_confirm "i3-msg exit"
    elif [[ "$XDG_CURRENT_DESKTOP" =~ "sway" ]]; then
        execute_with_confirm "swaymsg exit && systemctl stop --user sway-session.target"
    elif [[ "$XDG_CURRENT_DESKTOP" =~ "Hyprland" ]]; then
        execute_with_confirm "hyprctl dispatch -- exit && systemctl stop --user hyprland-session.target"
    fi
}

# Variable passed to rofi
options="${shutdown}\n${reboot}\n${lock}\n${suspend}\n${logout}"

chosen="$(echo -e "$options" | $rofi_command -p " Uptime: $uptime_command " -dmenu -selected-row 2)"

case $chosen in
    "$shutdown") systemctl poweroff ;;
    "$reboot") systemctl reboot ;;
    "$lock") loginctl lock-session ;;
    "$suspend") systemctl suspend ;;
    "$logout") logout ;;
esac
