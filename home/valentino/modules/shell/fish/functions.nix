pkgs: let
  _git = "${pkgs.git}/bin/git";
  _gs = "${pkgs.ghostscript}/bin/gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite";
  _ffmpeg = "${pkgs.ffmpeg}/bin/ffmpeg";
  _lualatex = "${pkgs.texliveTeTeX}/bin/lualatex -shell-escape --interaction=nonstopmode --file-line-error";
in {
  fish_prompt = {
    body = ''
      set -l last_status $status
      set -l cyan (set_color -o cyan)
      set -l yellow (set_color -o yellow)
      set -g red (set_color -o red)
      set -g blue (set_color -o blue)
      set -l green (set_color -o green)
      set -l green_bold (set_color -o green --bold)
      set -l blue_bold (set_color -o blue --bold)
      set -l white (set_color -o white)
      set -g normal (set_color normal)
      set -l cwd (prompt_pwd)

      echo -n "$white╭─ $green_bold"
      printf '%s ' (whoami)

      echo -n "$red@ $blue_bold$hostname   $cwd$normal"
      __fish_git_prompt " (%s)"
      echo -e ""
      echo -n "$white╰─$yellow λ $normal"
    '';
  };

  fish_right_prompt = {
    body = ''
      if test $CMD_DURATION
        echo "$CMD_DURATION 1000" | awk '{printf "%.3fs", $1 / $2}'
      end
      echo " "
    '';
  };

  # sudo dd bs=4M if=<input> of=<output> conv=fdatasync status=progress
  # Check with cmp (If you get an EOF message then the files are identical, otherwise cmp will tell you at which byte they differ.)
  burn-iso = {
    body = ''
      if [ -z "$argv" ] || test -z "$argv[1]" || test -z "argv[2]"
        echo -e "Arguments needed not supplied\nUsage:\n\tburn-iso {source} {dest}"
      else
        time sudo ${pkgs.coreutils}/bin/dd bs=4M if="$argv[1]" of="$argv[2]" conv=fdatasync status=progress
      end
    '';
  };

  # Adapted from: https://gist.github.com/junegunn/f4fca918e937e6bf5bad
  log = {
    body = ''
      if ${_git} rev-parse --git-dir > /dev/null 2>&1
        ${_git} log --graph --format="%C(auto)%h%d %s %C(white)%C(bold)%cr" --color=always | \
            fzf --ansi \
                --reverse \
                --tiebreak=index \
                --no-sort \
                --preview 'f() { set -- $(echo -- "$@" | grep -o "[a-f0-9]\{7\}"); [ $# -eq 0 ] || ${_git} show --color=always $1; }; f {}' \
                --bind "alt-j:preview-down,alt-k:preview-up,ctrl-f:preview-page-down,ctrl-b:preview-page-up" \
                --bind "ctrl-m:execute:echo {} | grep -o '[a-f0-9]\{7\}' | head -1 |  xargs -I % sh -c 'git show --color=always % | less -R'" \
                --preview-window=right:60%
      end
    '';
  };

  rank = {
    body = ''
      set -l green (set_color -o green)
      set -g normal (set_color normal)
      if ${_git} rev-parse --git-dir > /dev/null 2>&1
         ${_git} shortlog -sn --no-merges && echo -e "★ $green $(${_git} rev-list --count HEAD) $normal commits so far"
      end
    '';
  };

  forget_last_command = {
    body = ''
      set last_typed_command (history --max 1)
      history delete --exact --case-sensitive $last_typed_command
      true
    '';
  };

  concatenate-pdf = {
    body = ''
      ${_gs} -sOUTPUTFILE=(path change-extension "" $argv[1])_concatenated.pdf $argv[1]
    '';
  };

  # Adapted from: https://gist.github.com/ahmed-musallam/27de7d7c5ac68ecbd1ed65b6b48416f9
  pdf-compress = {
    body = ''
      ${_gs} -q -dSAFER -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dEmbedAllFonts=true -dSubsetFonts=true -dColorImageDownsampleType=/Bicubic -dColorImageResolution=144 -dGrayImageDownsampleType=/Bicubic -dGrayImageResolution=144 -dMonoImageDownsampleType=/Bicubic -dMonoImageResolution=144 -sOutputFile=(path change-extension "" $argv[1])_compressed.pdf $argv[1]
    '';
  };

  compile = {
    body = ''
      if [ -z "$argv" ];
        echo "No arguments supplied"
        return
      else
        echo "1° round"
        ${_lualatex} "$argv[1]" > /dev/null 2>&1
        echo "2° round"
        ${_lualatex} "$argv[1]" > /dev/null 2>&1
        echo "3° round"
        ${_lualatex} "$argv[1]" > /dev/null 2>&1
        echo "DONE!"
        return
      end
    '';
  };

  emptytrash = {
    body = ''
      rm ~/.local/share/Trash/files
      rm ~/.local/share/Trash/info
      mkdir ~/.local/share/Trash/info
      mkdir ~/.local/share/Trash/files
    '';
  };

  mkcd = {
    body = ''
      mkdir "$argv[1]" && cd "$argv[1]"
    '';
  };

  hostname2ip = {
    body = ''
      ping -c 1 "$argv[1]" | egrep -m1 -o "[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}"
    '';
  };

  ex = {
    body = ''
      for file in $argv
        if test -f $file
          echo -s "Extracting " (set_color --bold blue) $file (set_color normal)
          switch $file
            case "*.tar"
              tar -xvf $file
            case "*.tar.bz2" "*.tbz2"
              tar --bzip2 -xvf $file
            case "*.tar.gz" "*.tgz"
              tar --gzip -xvf $file
            case "*.bz" "*.bz2"
              bunzip2 $file
            case "*.gz"
              gunzip $file
            case "*.rar"
              unrar x $file
            case "*.zip"
              #unzip -uo $file -d (basename $file .zip)
              unzip $file
            case "*.Z"
              uncompress $file
            case "*.pax"
              pax -r < $file
            case "*.zstd"
              unzstd $file
            case '*'
              echo "Extension not recognized, cannot extract $file"
          end
        else
          echo "$file is not a valid file"
        end
      end
    '';
  };

  # Ask nixos why system want X package
  why-depends = {
    body = ''
      if [ -z "$argv" ];
        echo "No argument supplied"
        return
      else
        nix-store --query --referrers $(which "$argv[1]")
      end
    '';
  };

  fcut = {
    body = ''
      if set -q "$argv"
        return 1
      end
      if test -z "$argv[1]"; or test -z "$argv[2]"; or test -z "$argv[3]"; or test -z "argv[4]"
        echo "Usage:"
        echo "fcut {input} {start} {end} {output}"
        return 1
      end
      ${_ffmpeg} -i "$argv[1]" -ss "$argv[2]"  -t "$argv[3]" -c:v copy -c:a copy "$argv[4]"
    '';
  };

  fconcat = {
    body = ''
      if set -q "$argv"
        return 1
      end
      if test -z "$argv[1]"; or test -z "$argv[2]"; or test -z "$argv[3]"
        echo "Parametri mancanti o assenti"
        echo "Uso:"
        echo "fconcat {file1} {file2} {file finale}"
        return 1
      end
      echo file "$argv[1]" >> mylist.txt
      echo file "$argv[2]" >> mylist.txt
      ${_ffmpeg} -f concat -safe 0 -i mylist.txt -c copy "$argv[3]" && rm mylist.txt
    '';
  };

  # Rebuild configuration / update flake.lock file
  # If git fails add : sudo git config --add safe.directory <directory>
  # You can pass `--option eval-cache false` to turn off caching so that Nix will always show you the error message instead of error: cached failure of attribute 'nixosConfigurations.default.config.system.build.toplevel'
  update = {
    body = ''
      set -l base_path $HOME/nix-config
      switch $argv
        case "--flake"
            nix flake update $base_path
        case "*"
          sudo nixos-rebuild switch --flake "$base_path/.#$hostname" -v -L --use-remote-sudo
      end
    '';
  };

  home-switch = {
    body = ''
      set -l base_path $HOME/nix-config
      home-manager switch --flake "$base_path/.#$USER@$hostname"
    '';
  };

  # [net]work [u]sage: check network usage stats
  netu = {
    body = ''
      set -l net_device (ip route | awk '/via/ {print $5}')
      set -l transmitted (ifconfig "$net_device" | awk '/TX packets/ {print $6$7}')
      set -l received (ifconfig "$net_device" | awk '/RX packets/ {print $6$7}')

      printf "%s\n" "$(tput bold)🔼 TRANSMITTED $(tput sgr0): $transmitted"
      printf "%s\n" "$(tput bold)🔽 RECEIVED    $(tput sgr0): $received"
    '';
  };
}
