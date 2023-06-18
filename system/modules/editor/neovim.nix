{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.editor.neovim;
in {
  options.system.modules.editor.neovim = {
    enable = mkEnableOption "Enable neovim";
  };
  config = mkIf cfg.enable {
    environment.systemPackages = [
      inputs.neovim-flake.packages.x86_64-linux.nvim
      pkgs.alejandra
      pkgs.cppcheck
      pkgs.nodePackages.bash-language-server
    ];
  };
}
