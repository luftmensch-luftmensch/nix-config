{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.git;
  inherit (config.valentino.modules.editors) neovim;
  undo-git = pkgs.writeScriptBin "undo-git" (builtins.readFile ./scripts/undo-git.sh);
  git-blame-someone-else = pkgs.writeScriptBin "git-blame-someone-else" (builtins.readFile ./scripts/git-blame-someone-else.sh);
in {
  options.valentino.modules.shell.git = {
    enable = mkEnableOption "main user git configuration";
  };

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
            editor =
              if neovim.enable
              then "nvim"
              else "";
          };

          init = {
            defaultBranch = "master";
          };

          merge = {
            conflictStyle = "zdiff3";
            stat = true;
          };

          column = {
            ui = "auto";
          };

          branch = {
            autoSetupMerge = true;
            sort = "-committerdate";
          };

          push = {
            autoSetupRemote = true;
          };

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

          commit = {
            gpgsign = true;
          };

          user = {
            signingkey = "~/.ssh/id_homelab.pub";
          };

          gpg = {
            format = "ssh";
          };
        };
        aliases = {
          aliases = "!git config --get-regexp '^alias\\.' | cut -c 7- | sed 's/ / = /'";
          # alias = "config --get-regexp alias\\.";
          f = "fetch";
          fuckit = "reset --hard";
          d = "diff";
          undo = "reset HEAD~1 --mixed";
          last = "!git log -1 HEAD --stat";

          # List all branches
          b = "rev-parse --abbrev-ref HEAD";
          br = "branch --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(contents:subject) %(color:green)(%(committerdate:relative)) [%(authorname)]' --sort=-committerdate";
          bra = "branch -vva";
          cb = "create-branch";
          db = "delete-branch";
          bcontains = "branch -a --contains";
          branches = "!git -c color.ui=always branch -a --sort=-committerdate | grep -Ev '(/HEAD|/(main|master))' | sed -r -e 's,^[\\* ]*,,' -e 's,remotes/[^/]+/,,'";

          # Taken from https://bhupesh.me/git-cake-when-is-my-readme-birthday/
          cake = "log --date=format:'%d %b %Y' --diff-filter=A --name-only --pretty='%n%C(yellow bold)🎂️ %ad%Creset by (%C(blue bold)%h%Creset)'";

          clh = "!f() { git clone $1 $(echo $1 | awk -F '/' '{print $4}'); }; f";
          # cls = "! f() { git clone git@github.com:$1/$2.git ; }; f";
          refresh = "pull --rebase --autostash origin HEAD";

          workon = "! f(){ git fetch && git checkout -b $1 origin/HEAD; }; f";
          cleanup-merged = "!f(){ git fetch && git branch --merged | grep -v '* ' | xargs git branch --delete; }; f";
          switch-branch = ''
            !f() { [ $# -gt 0 ] && exec git switch "$@"; branch=$( git branches 2>/dev/null | fzf +s --no-multi --prompt 'branches» ' ) && git switch "$branch"; }; f
          '';
          wip = "!git commit -m \"WIP: Changes in $( echo $( git diff --cached --name-only ) )\"";
          stats = ''
            !f(){
              default_since_date=$(date -d "1 week ago" +%Y-%m-%d); \
              default_until_date=$(date +%Y-%m-%d); \
              default_branch_name="$(basename $(git symbolic-ref --short refs/remotes/origin/HEAD))"; \

              read -rp "Since  [$default_since_date]: " since_date; \
              since_date=''${since_date:-$default_since_date}; \

              read -rp "Until  [$default_until_date]: " until_date; \
              until_date=''${until_date:-$default_until_date}; \

              read -rp "Branch [main]: " branch_name; \
              branch_name=''${branch_name:-$default_branch_name}; \

              days=$((($(date -d "$until_date" +%s) - $(date -d "$since_date" +%s)) / 86400 + 1)); \
              git log --since="$since_date" --until="$until_date" --branches="$branch_name" --oneline --numstat "$branch_name" | awk \
              -v days="$days" '$1 ~ /^[0-9]+$/ && $2 ~ /^[0-9]+$/ {; \
                  added+=$1; \
                  deleted+=$2; \
                } END {; \
                print "\nFrom " "'"$since_date"'" " to " "'"$until_date"'" " (" days " days)\n"; \
                print "🟢 Added lines:   " added; \
                print "🔴 Deleted lines: " deleted "\n"; \
                if (days > 0) {; \
                  print "🚀 Average lines changed per day: " (added+deleted)/days; \
                }; \
              }';\
            }; f
          '';
        };
      };

      lazygit.enable = true;
    };

    home.packages = [undo-git git-blame-someone-else];
  };
}
