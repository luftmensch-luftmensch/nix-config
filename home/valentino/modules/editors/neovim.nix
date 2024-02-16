{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.editors.neovim;
in {
  options.valentino.modules.editors.neovim = {
    enable = mkEnableOption "neovim";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs;
      [
        alejandra
        cppcheck
        nodePackages.bash-language-server
      ]
      ++ (with inputs.neovim-flake.packages.${pkgs.system}; [
        nvim
      ]);
  };
}
