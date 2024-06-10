{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.shell.git;
  inherit (config.valentino.modules.editors) neovim;
  undo-git = pkgs.writeScriptBin "undo-git" (builtins.readFile ./scripts/undo-git.sh);
  git-blame-someone-else = pkgs.writeScriptBin "git-blame-someone-else" ''
    if [ $# -ne 2 ]; then
      >&2 echo "Usage: $0 <author> <commit>"
      exit 1
    fi

    AUTHOR=$1
    AUTHOR_NAME=$(echo $AUTHOR | perl -wlne '/^(.*?)\s*<.*>$/ and print $1')
    AUTHOR_EMAIL=$(echo $AUTHOR | perl -wlne '/^.*\s*<(.*)>$/ and print $1')
    COMMIT=$(git rev-parse --short $2)

    {
      GIT_SEQUENCE_EDITOR="sed -i -e 's/^pick $COMMIT/edit $COMMIT/'" git rebase -i $COMMIT~1^^
      GIT_COMMITTER_NAME="$AUTHOR_NAME" GIT_COMMITTER_EMAIL="$AUTHOR_EMAIL" git commit --amend --no-edit --author="$AUTHOR"
      git rebase --continue
    } &> /dev/null

    echo "$AUTHOR_NAME is now the author of $COMMIT. You're officially an asshole.";
  '';
in
{
  options.valentino.modules.shell.git.enable = mkEnableOption "main user git configuration";

  config = mkIf cfg.enable {
    programs = {
      git = {
        enable = true;
        userEmail = "valentinobocchetti59@gmail.com";
        userName = "luftmensch-luftmensch";
        # https://linuxize.com/post/gitignore-ignoring-files-in-git/
        # https://www.toptal.com/developers/gitignore
        ignores = [
          # ------- compiled source ------- #
          "*.exe"
          "*.o"
          "*.out"
          "*.pyc"

          # ------------- logs ------------ #
          "*.log"
          ".log"

          # --------- OS generated -------- #
          ".DS_Store"
          ".DS_Store?"
        ];

        extraConfig = {
          core = {
            # avoid git status showing all your files as modified because of the
            # automatic EOL conversion done when cloning a Unix-based EOL Git repo to a Windows one
            autocrfl = false;
            editor = if neovim.enable then "nvim" else "";
            # pager = "bat --theme=default --paging=always --tabs=4 --wrap=never --style=plain";
          };

          init.defaultBranch = "master";

          merge = {
            conflictStyle = "zdiff3";
            stat = true;
          };

          column.ui = "auto";

          branch = {
            autoSetupMerge = true;
            sort = "-committerdate";
          };

          push.autoSetupRemote = true;

          diff = {
            algorithm = "histogram";
            mnemonicPrefix = true;
          };

          # https://git-scm.com/book/en/v2/Git-Tools-Rerere
          rerere.enable = true;

          color.status = {
            added = "green";
            untracked = "red";
            changed = "yellow";
          };

          commit.gpgsign = true;

          user.signingkey = "~/.ssh/id_homelab.pub";

          gpg.format = "ssh";
        };

        aliases = {
          aliases = "!git config --get-regexp '^alias\\.' | cut -c 7- | sed 's/ / = /'";
          amend = "!git commit --amend";
          st = "status -sb";
          co = "checkout";
          ci = "commit -m";
          aa = "add -A";
          ac = "!git add -A && git commit";
          fuckit = "reset --hard";
          undo = "reset HEAD~1 --mixed";
          last = "log -1 HEAD --stat";
          last-updated = "!git log --name-only --format='' --since '1 week ago' | perl -lne 'print unless $seen{$_}++'";

          redo = "reset --hard HEAD@{1}";
          rv = ''!sh -c 'git commit --amend --no-edit --trailer "Reviewed-by: $*"' - '';

          branches = "branch -vva --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(contents:subject) %(color:green)(%(committerdate:relative)) [%(authorname)]' --sort=-committerdate";

          # Taken from https://bhupesh.me/git-cake-when-is-my-readme-birthday/
          cake = "log --date=format:'%d %b %Y' --diff-filter=A --name-only --pretty='%n%C(yellow bold)🎂️ %ad%Creset by (%C(blue bold)%h%Creset)'";

          refresh = "pull --rebase --autostash origin HEAD";

          workon = "!git fetch && git switch -c";
          cleanup-merged = "!git fetch && git branch --merged | grep -v '* ' | xargs git branch --delete";
          wip = "!git commit -m \"WIP: Changes in $( echo $( git diff --cached --name-only ) )\"";
          stats = ''
            !f(){
              default_since="$(date -d "1 week ago" +%d-%m-%Y)"; \
              default_until=$(date +%d-%m-%Y); \
              default_branch="$(basename $(git symbolic-ref --short refs/remotes/origin/HEAD))"; \

              read -rp "Since  ($default_since): " since; \
              since=''${since:-$default_since}; \

              read -rp "Until  ($default_until): " until; \
              until=''${until:-$default_until}; \

              read -rp "Branch [main]: " branch_name; \
              branch_name=''${branch_name:-$default_branch}; \
              formatted_since=$(echo $since | awk -F'-' '{printf("%s/%s/%s", $3, $2, $1)}'); \
              formatted_until=$(echo $until | awk -F'-' '{printf("%s/%s/%s", $3, $2, $1)}'); \

              days=$((($(date -d "$formatted_until" +%s) - $(date -d "$formatted_since" +%s)) / 86400 + 1)); \

              git log --since="$formatted_since" --until="$formatted_until" --branches="$branch_name" --oneline --numstat "$branch_name" | awk \
              -v days="$days" '$1 ~ /^[0-9]+$/ && $2 ~ /^[0-9]+$/ {; \
                  added+=$1; \
                  deleted+=$2; \
                } END {; \
                print "\nFrom " "'"$since"'" " to " "'"$until"'" " (" days " days)\n"; \
                print "🟢 Added lines:   " added; \
                print "🔴 Deleted lines: " deleted "\n"; \
                if (days > 0) {; \
                  print "🚀 Average lines changed per day: " (added+deleted)/days; \
                }; \
              }';\
            }; f
          '';

          rank = "!f(){ if [ $(git rev-parse --git-dir 2> /dev/null) ]; then git shortlog -sn --no-merges && echo -e \"\\033[32;40;1m★  $(git rev-list --count HEAD) \\033[0m commits so far\"; fi };f";

          # Adapted from: https://gist.github.com/junegunn/f4fca918e937e6bf5bad
          ll = ''
            !f(){
              if [ $(git rev-parse --git-dir 2> /dev/null) ]; then \
                  git log --graph --format="%C(auto)%h%d %s %C(white)%C(bold)%cr" --color=always | \
                  fzf --ansi --reverse --tiebreak=index --no-sort --preview 'f() { set -- $(echo -- "$@" | grep -o "[a-f0-9]\{7\}"); [ $# -eq 0 ] || git show --color=always $1; }; f {}' --bind "alt-j:preview-down,alt-k:preview-up,ctrl-f:preview-page-down,ctrl-b:preview-page-up" --bind "ctrl-m:execute:echo {} | grep -o '[a-f0-9]\{7\}' | head -1 |  xargs -I % sh -c 'git show --color=always % | bat --theme=default --paging=always --tabs=4 --wrap=never --style=plain'" --preview-window=right:60%
              fi; \
            }; f
          '';
        };
      };

      lazygit.enable = true;

      gh = {
        enable = true;
        settings = {
          aliases = {
            co = "pr checkout";
            pv = "pr view";
          };

          editor = if neovim.enable then "nvim" else "";
        };
      };
    };

    home.packages = [
      undo-git
      git-blame-someone-else
      pkgs.oh-my-git
    ];
  };
}
