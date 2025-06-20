{ lib, pkgs }:
let
  _curl = "${lib.getExe pkgs.curl}";
  _docker = "${lib.getExe pkgs.docker}";
  _gpg = "${lib.getExe pkgs.gnupg} --keyserver-options auto-key-retrieve";
  _nmcli = "${pkgs.networkmanager}/bin/nmcli device";
  _dig = lib.getExe' pkgs.dnsutils "dig";
in
{
  mkdir = "mkdir -p ";
  free = "free -gt";
  exe = "chmod +x ";
  rm = "rm -rf ";

  ",," = "cd ..";
  "..." = "cd ../..";
  "...." = "cd ../../..";
  "....." = "cd ../../../..";
  q = "exit";

  ipe = "${_curl} ipinfo.io/ip";
  myip = "${_dig} @resolver4.opendns.com myip.opendns.com +short";

  weather = "${_curl} -s wttr.in/naples?format=%l++%m++%C+%c+%t+%w++%p";

  utftest = "${_curl} https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-demo.txt";
  parrot = "${_curl} parrot.live";
  rr = "${_curl} -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash";

  # LICENSE
  gpl = "${_curl} https://www.gnu.org/licenses/gpl-3.0.txt -o LICENSE";
  agpl = "${_curl} https://www.gnu.org/licenses/agpl-3.0.txt -o LICENSE";
  mit = "${_curl} https://mit-license.org/license.txt -o LICENSE";

  webcam = "ffplay /dev/video0";

  jctl = "journalctl -p 3 -xb";
  errors = "journalctl -p err..alert";
  clear = "printf '\\033[2J\\033[3J\\033[1;1H'";
  clear-journaling = "sudo journalctl --rotate && sudo journalctl --vacuum-time=1s";
  journaling-disk-size = "journalctl --disk-usage";
  reload-sddm = "sudo systemctl restart display-manager.service";

  ps = "ps auxf | less";
  psgrep = "ps aux | grep -v grep | grep -i -e VSZ -e";
  running = "systemctl --type=service";
  wget = "wget -c";

  reboot-to-bios = "sudo systemctl reboot --firmware-setup";

  list-generation = "sudo nix-env -p /nix/var/nix/profiles/system --list-generations";
  next-cg = "sudo systemctl status nix-gc.timer";
  system-diff-generation = "nix profile diff-closures --profile /nix/var/nix/profiles/system";
  home-diff-generation = "nix profile diff-closures --profile ~/.local/state/nix/profiles/home-manager";
  nix-show-inputs = "nix flake archive --json | jq '.path, ( .inputs | to_entries[] | {\"input\": .key, \"path\": .value.path})'";

  ls = "${lib.getExe pkgs.eza} --icons --color=always --group-directories-first";
  sl = "ls";
  la = "ls -a";
  ll = "ls -l";
  "l." = "ls -a --group-directories-first | egrep '^\.'";
  lll = "ls --header --git --classify --long --binary --group --time-style=long-iso --links --all --sort=name";
  tree = "ls -aT";

  used-port = "echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq";
  open-port = "used-port";

  yt = "${lib.getExe pkgs.yt-dlp}";
  yt-aac = "yt --extract-audio --audio-format aac ";
  yt-best = "yt --extract-audio --audio-format best ";
  yt-flac = "yt --extract-audio --audio-format flac ";
  yt-m4a = "yt --extract-audio --audio-format m4a ";
  yt-mp3 = "yt --extract-audio --audio-format mp3 ";
  yt-opus = "yt --extract-audio --audio-format opus ";
  yt-vorbis = "yt --extract-audio --audio-format vorbis ";
  yt-wav = "yt --extract-audio --audio-format wav ";
  ytv-best = "yt -f bestvideo+bestaudio ";
  yt-list-format = "yt -F";

  buytime = "${pkgs.coreutils}/bin/dd if=/dev/urandom of=homework.pdf bs=1K count=4";

  # ----------- CPU settings ----------#
  cpuinfo = "watch -n .1 \"grep '^[c]pu MHz' /proc/cpuinfo\"";

  # ----------- Network ----------#
  nmcon = "${_nmcli} wifi connect";
  nmrs = "${_nmcli} wifi rescan";
  nmls = "${_nmcli} wifi list";
  nmst = "${_nmcli} status";

  ip = "ip -color=auto -br";
  wifikey = "sudo grep -r \"^psk=\" /etc/NetworkManager/system-connections/";

  # ------ Systemd-analyze ------- #
  sapu = "systemd-analyze --user plot > systemd-analyze-user.svg";
  sap = "systemd-analyze plot > systemd-analyze-user.svg";

  # Docker related
  dcu = "${_docker} compose up -d";
  dcd = "${_docker} compose down";
  dps = "${_docker} ps";

  # Misc
  v = "vim";

  getpass = "${lib.getExe pkgs.openssl} rand -base64 20";
  # verify signature for isos
  gpg-check = "${_gpg} --verify";
  # receive the key of a developer
  gpg-retrieve = "${_gpg} --receive-keys";

  adb-force-restart = "sudo adb kill-server; sudo adb start-server";

  installation-date = "stat -c %w /";

  mime = "xdg-mime query filetype";
  font-family = "fc-list : family | ${lib.getExe pkgs.fzf}";

  # make sudo use aliases
  sudo = "sudo ";
}
