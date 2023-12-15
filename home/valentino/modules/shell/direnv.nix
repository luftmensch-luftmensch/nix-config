{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.direnv;
  cfgBash = config.valentino.modules.shell.bash;
  cfgZsh = config.valentino.modules.shell.zsh;
  cfgFish = config.valentino.modules.shell.fish;
in {
  options.valentino.modules.shell.direnv = {
    enable = mkEnableOption "direnv and extensions";
  };

  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration =
        if cfgBash.enable
        then true
        else false;
      enableZshIntegration =
        if cfgZsh.enable
        then true
        else false;

      # enableFishIntegration =
      #   if cfgFish.enable
      #   then true
      #   else false;
      nix-direnv.enable = true; # better than lorri?
    };

    # services.lorri.enable = true;
  };
}
