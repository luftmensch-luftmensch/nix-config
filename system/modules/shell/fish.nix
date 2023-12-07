{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.shell.fish;
in {
  options.system.modules.shell.fish = {
    enable = mkEnableOption "Fish - The friendly interactive shell";
  };

  config = mkIf cfg.enable {
    programs.fish = {
      enable = true;
      vendor = {
        completions.enable = true;
        config.enable = true;
      };
    };
  };
}
