{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.js;
in {
  options.valentino.modules.dev.js = {
    enable = mkEnableOption "javascript language tools";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nodejs
      node2nix
      yarn
      nodePackages.typescript-language-server
    ];

    # Needed to quickly install borked modules (e.g. amplify, *coff*) in home
    # Not recommended, though
    home.sessionVariables = {
      PATH = "$HOME/.mutable_node_modules/bin:$PATH";
      NODE_PATH = "$HOME/.mutable_node_modules/lib/node_modules";
    };
  };
}
