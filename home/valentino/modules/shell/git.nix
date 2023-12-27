{
  options,
	pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.git;
	undo-git = pkgs.writeScriptBin "undo-git" (builtins.readFile ./scripts/undo-git.sh);
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
      # config = {

      # };
      extraConfig = {
        core = {
          # avoid git status showing all your files as modified because of the
          # automatic EOL conversion done when cloning a Unix-based EOL Git repo to a Windows one
          autocrfl = false;
          # FIXME: Hardcoded config
          editor = "nvim";
        };

        init = {
          defaultBranch = "master";
        };

        merge = {
          conflictStyle = "diff3";
          stat = true;
        };

        branch = {
          autoSetupMerge = true;
        };

        push = {
          autoSetupRemote = true;
        };

        diff = {
          mnemonicPrefix = true;
        };

        color.status = {
          added = "green";
          untracked = "red";
          changed = "yellow";
        };
      };
    };

		home.packages = with pkgs; [
			undo-git
		];

  };
}
