#!/usr/bin/env bash

# Configurations folder
theme="$HOME/.config/rofi/themes/powermenu"

uptime_command=$(awk '{d=int($1/86400); h=int(($1%86400)/3600); m=int(($1%3600)/60); printf("%s%s%d minutes\n", d?d" days, ":"", h?h" hours, ":"", m)}' /proc/uptime)
rofi_command="rofi -theme $theme"

# Options
shutdown="󰐥"
reboot="󰑓"
lock="󰌾"
suspend=" "
logout="󰍃"

logout() {
    if [[ "$DESKTOP_SESSION" =~ "qtile" ]]; then
        qtile cmd-obj -o cmd -f shutdown
    elif [[ "$DESKTOP_SESSION" =~ "i3" ]]; then
        i3-msg exit
    elif [[ "$XDG_CURRENT_DESKTOP" =~ "sway" ]]; then
        swaymsg exit && systemctl stop --user sway-session.target
    elif [[ "$XDG_CURRENT_DESKTOP" =~ "Hyprland" ]]; then
        hyprctl dispatch -- exit && systemctl stop --user hyprland-session.target
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
