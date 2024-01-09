pkgs: {
  mkdir = "mkdir -p ";
  free = "free -gt";
  exe = "chmod +x ";
  untar = "tar -zxvf ";
  rm = "rm -rf ";

  ",," = "cd ..";
  "..." = "cd ../..";
  "...." = "cd ../../..";
  "....." = "cd ../../../..";
  q = "exit";
  ":q" = "exit";

  ipe = "${pkgs.curl}/bin/curl ipinfo.io/ip";
  myip = "${pkgs.curl}/bin/curl ipv4.icanhazip.com";

  weather = "${pkgs.curl}/bin/curl wttr.in/naples";
  wttr = "${pkgs.curl}/bin/curl -s wttr.in/naples | head -n 7 | tail -n 6; printf '\n'"; # https://wttr.in/?format=%l++%m++%C+%c+%t+%w++%p

  utftest = "${pkgs.curl}/bin/curl https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-demo.txt";
  parrot = "${pkgs.curl}/bin/curl parrot.live";
  rr = "${pkgs.curl}/bin/curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash";

  # LICENSE
  gpl = "${pkgs.curl}/bin/curl https://www.gnu.org/licenses/gpl-3.0.txt";
  agpl = "${pkgs.curl}/bin/curl https://www.gnu.org/licenses/agpl-3.0.txt";
  mit = "${pkgs.curl}/bin/curl https://mit-license.org/license.txt";

  jctl = "journalctl -p 3 -xb";
  clear = "printf '\\033[2J\\033[3J\\033[1;1H'";
  clear-journaling = "sudo journalctl --rotate && sudo journalctl --vacuum-time=1s";
  journaling-disk-size = "journalctl --disk-usage";
  reload-sddm = "sudo systemctl restart display-manager.service";

  ps = "ps auxf | less";
  psgrep = "ps aux | grep -v grep | grep -i -e VSZ -e";
  running = "systemctl --type=service";
  wget = "wget -c --hsts-file=$HOME/.cache/wget-hsts";

  reboot-to-bios = "sudo systemctl reboot --firmware-setup";

  list-generation = "sudo nix-env -p /nix/var/nix/profiles/system --list-generations";
  next-cg = "sudo systemctl status nix-gc.timer";
  nix-diff-generation = "nix profile diff-closures --profile /nix/var/nix/profiles/system";
  hm-diff-generation = "nix profile diff-closures --profile ~/.local/state/nix/profiles/home-manager";
  nix-show-inputs = "nix flake archive --json | jq '.path, ( .inputs | to_entries[] | {\"input\": .key, \"path\": .value.path})'";

  ls = "${pkgs.eza}/bin/eza --icons --color=always --group-directories-first";
  sl = "ls";
  la = "ls -a";
  ll = "ls -l";
  "ll." = "ls --header --git --classify --long --binary --group --time-style=long-iso --links --all --all --sort=name";
  "l." = "ls -a --group-directories-first | egrep '^\.'";
  lll = "ls --header --git --classify --long --binary --group --time-style=long-iso --links --all --sort=name";
  tree = "ls -aT";

  branch = "git branch";
  clone = "git clone";
  commit = "git commit -m";
  gs = "git status -sb";
  gf = "git fetch";
  gp = "git pull";
  gP = "git push origin";
  ga = "git add .";
  gr = "git reset --hard && git clean -fd";

  git-diff = "git log --oneline --color=always | fzf --reverse -i --pointer=\"▶\"   --info=inline --border=rounded --cycle --ansi --preview=\"echo {} | cut -d ' ' -f 1 | xargs -I @ sh -c 'git log --pretty=medium -n 1 @; git diff @^ @' | bat --color=always\" | cut -d ' ' -f 1 | xargs git log --pretty=short -n 1";

  used-port = "echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq";
  open-port = "used-port";

  yt = "${pkgs.yt-dlp}/bin/yt-dlp";
  yta-aac = "yt --extract-audio --audio-format aac ";
  yta-best = "yt --extract-audio --audio-format best ";
  yta-flac = "yt --extract-audio --audio-format flac ";
  yta-m4a = "yt --extract-audio --audio-format m4a ";
  yta-mp3 = "yt --extract-audio --audio-format mp3 ";
  yta-opus = "yt --extract-audio --audio-format opus ";
  yta-vorbis = "yt --extract-audio --audio-format vorbis ";
  yta-wav = "yt --extract-audio --audio-format wav ";
  ytv-best = "yt -f bestvideo+bestaudio ";
  yt-list-format = "yt -F";

  buytime = "${pkgs.coreutils}/bin/dd if=/dev/urandom of=homework.pdf bs=1K count=4";

  # ----------- CPU settings ----------#
  cpuinfo = "watch -n .1 \"grep '^[c]pu MHz' /proc/cpuinfo\"";

  # ----------- Network ----------#
  nmcon = "${pkgs.networkmanager}/bin/nmcli device wifi connect";
  nmls = "${pkgs.networkmanager}/bin/nmcli device wifi list";
  nmst = "${pkgs.networkmanager}/bin/nmcli device status";

  ip = "ip -color=auto -br";
  wifikey = "sudo grep -r \"^psk=\" /etc/NetworkManager/system-connections/";

  # ------ Systemd-analyze ------- #
  sapu = "systemd-analyze --user plot > systemd-analyze-user.svg";
  sap = "systemd-analyze plot > systemd-analyze-user.svg";

  # Docker related
  dcu = "${pkgs.docker}/bin/docker compose up -d";
  dcd = "${pkgs.docker}/bin/docker compose down";
  dps = "${pkgs.docker}/bin/docker ps";

  # Misc
  vim = "nvim";
  v = "vim";

  getpass = "${pkgs.openssl}/bin/openssl rand -base64 20";
  # verify signature for isos
  gpg-check = "${pkgs.gnupg}/bin/gpg --keyserver-options auto-key-retrieve --verify";
  # receive the key of a developer
  gpg-retrieve = "${pkgs.gnupg}/bin/gpg --keyserver-options auto-key-retrieve --receive-keys";

  adb-force-restart = "sudo adb kill-server; sudo adb start-server";
}
