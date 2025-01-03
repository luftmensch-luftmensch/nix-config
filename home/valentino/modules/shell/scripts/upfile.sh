#!/usr/bin/env bash

log() {
    tostdout () {
        printf "%*s%b%s %s %s\033[0m%b" "$INDENT" '' "${COLOR}" "${HEADER}" "${SEPARATOR}" "${MESSAGE}" "${NEWLINE:-\n}";
    }

    # No message given
    if [[ $# -eq 0 ]]; then
        log E "No parameters were given to the logmsg function!";
        return 0
    fi

    FORMAT="${1}"; shift;
    MESSAGE="${1}"; shift;
    INDENT=${INDENT:-0}

    shopt -s nocasematch
    case "$FORMAT" in
        'E')  HEADER="[ERR] "; COLOR="\033[0;31m"; SEPARATOR="»"; tostdout ;;
        'W')  HEADER="[WARN]"; COLOR="\033[0;33m"; SEPARATOR="»"; tostdout ;;
        'I')  HEADER="[INFO]"; COLOR="\033[0;32m"; SEPARATOR="»"; tostdout ;;
        *) log E "Internal $(basename "$PROGNAME") error: format ‹${FORMAT}› does not exist!";;
    esac
    shopt -u nocasematch
}


if [ $# -eq 0 ]; then
    # If no argument is provided, read input from stdin
    log I "No file specified. Reading from stdin (Press Control-D when DONE)"
    temp_file=$(mktemp)
    cat >"$temp_file"
    file_to_upload="$temp_file"
else
    file_to_upload="$1"
fi

if [[ ! -f "$file_to_upload" && ! -p "$file_to_upload" ]]; then
    log E "Error: File '$file_to_upload' does not exist."
    exit 1
fi

result=$(mktemp)

if curl -s -F "file=@$file_to_upload" http://0x0.st -o "$result"; then
    log I "File uploaded successfully."
    wl-copy < "$result"
    log I "Link copied to clipboard."
else
    log I "Error: Failed to upload the file."
fi

# Clean up temporary file if it was created
[ -n "$temp_file" ] && rm -f "$temp_file"
[ -n "$result" ] && rm -f "$result"
