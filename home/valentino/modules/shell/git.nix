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
    programs.git = {
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
        };

        init = {
          defaultBranch = "master";
        };

        merge = {
          conflictStyle = "diff3";
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
        # Taken from https://bhupesh.me/git-cake-when-is-my-readme-birthday/
        cake = "log --date=format:'%d %b %Y' --diff-filter=A --name-only --pretty='%n%C(yellow bold)🎂️ %ad%Creset by (%C(blue bold)%h%Creset)'";
      };
    };

    programs.lazygit = {
      enable = true;
    };

    home.packages = [
      undo-git
      git-blame-someone-else
    ];
  };
}
