{ config, lib, ... }:
with lib;
let
  cfg = config.valentino.modules.shell.direnv;
  inherit (config.valentino.modules.shell) bash zsh;
in
{
  options.valentino.modules.shell.direnv.enable = mkEnableOption "direnv and extensions";

  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration = bash.enable;
      enableZshIntegration = zsh.enable;
      nix-direnv.enable = true;
    };
  };
}
