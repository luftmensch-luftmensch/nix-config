{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.vscode;
	configDir = "${config.home.homeDirectory}/nix-config/home/valentino/modules/editors/vscode";
in {
  options.valentino.modules.editors.vscode = {
    enable = mkEnableOption "vscode";
  };

  config = mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions; [
        mkhl.direnv
        bbenoist.nix
        vscodevim.vim
        gruntfuggly.todo-tree
        dart-code.dart-code
        dart-code.flutter
      ];
      keybindings = [
        {
          key = "ctrl+shift+e";
          command = "workbench.action.toggleSidebarVisibility";
        }
      ];
    };

		home.file.".config/VSCodium/User/settings.json" = {
			enable = true;
			source = config.lib.file.mkOutOfStoreSymlink "${configDir}/settings.json";
		};
  };
}
