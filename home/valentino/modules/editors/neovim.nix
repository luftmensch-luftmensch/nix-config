{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.valentino.modules.editors.neovim;
in
{
  options.valentino.modules.editors.neovim.enable = mkEnableOption "neovim";

  config = mkIf cfg.enable {
    home.packages = with inputs.neovim-flake.packages.${pkgs.stdenv.hostPlatform.system}; [ nvim ];
  };
}
