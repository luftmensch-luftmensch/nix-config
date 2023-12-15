{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.valentino.modules.shell.fish;
  aliases = import ./aliases.nix;
in {
  options.valentino.modules.shell.fish = {
    enable = mkEnableOption "Fish - The friendly interactive shell";
  };

  config = mkIf cfg.enable {
    programs.fish = {
      enable = true;
      shellInit = (import ./init.nix).settings;

      shellAbbrs = {
        pc = "valentino@192.168.1.171";
        home = "valentino@192.168.1.203";
      };

      shellAliases = import ../aliases.nix pkgs;
      functions = import ./functions.nix pkgs;
    };
  };
}
