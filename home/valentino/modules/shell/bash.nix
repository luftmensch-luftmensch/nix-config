{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.bash;
in {
  options.valentino.modules.shell.bash = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.bash = {
      enable = true;

      initExtra = ''
        git_branch() {
          git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)\ /';
        }

        PS1="╭─ \[\e[1;32m\]\u \[\e[1;31m\]@ \[\e[1;36m\]\h \[\e[1;34m\]\w \$(git_branch) \[\e[0;37m\]\n╰─ \[\e[0;33m\]λ \[\e[0m\]"

        PS2="\[\033[32m\]  > \[\e[0m\]"

        emptytrash(){
        	 rm ~/.local/share/Trash/files
        	 rm ~/.local/share/Trash/info
        	 mkdir ~/.local/share/Trash/info
        	 mkdir ~/.local/share/Trash/files
        }

        ex () {
          if [ -f "$1" ] ; then
          case $1 in
              *.tar.bz2)   tar xjf "$1"   ;;
              *.tar.gz)    tar xzf "$1"   ;;
              *.bz2)       bunzip2 "$1"   ;;
              *.rar)       unrar x "$1"   ;;
              *.gz)        gunzip "$1"    ;;
              *.tar)       tar xf "$1"    ;;
              *.tbz2)      tar xjf "$1"   ;;
              *.tgz)       tar xzf "$1"   ;;
              *.zip)       unzip "$1"     ;;
              *.Z)         uncompress "$1" ;;
              *.7z)        7z x "$1"      ;;
              *.deb)       ar x "$1"      ;;
              *.tar.xz)    tar xf "$1"    ;;
              *.tar.zst)   unzstd "$1"    ;;
              *)           echo "'$1' cannot be extracted via ex()" ;;
              esac
            else
                echo "'$1' is not a valid file"
          fi
        }

        hostname2ip() {
          ping -c 1 "$1" | egrep -m1 -o '[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}'
        }

        function spin() {
            while :;do for s in / - \\ \|; do printf "\r$s ...building...";sleep 0.2;done;done
        }

        function run_or_exit() {
          "$@" || exit $?
        }
      '';
      historyControl = [
        "ignorespace"
        "ignoredups"
      ];
      historyFile = "\${XDG_STATE_HOME}/bash/history";
      historyFileSize = 10000;
      shellOptions = [
        "histappend"
        "checkwinsize"
        "extglob"
        "globstar"
        "checkjobs"
        "autocd"
        "cdspell"
        "cmdhist"
        "expand_aliases"
        "globstar"
        "dotglob"
      ];

      # To list all the defined function use: declare -f | grep -E '^[^ ]+ \(\) $'
      shellAliases = import ./aliases.nix pkgs;
    };

    home.packages = [pkgs.beautysh];
  };
}
