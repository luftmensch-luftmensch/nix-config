{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.python;

  pyEnv = pkgs.python3.withPackages (ps: with ps; [flake8 python-lsp-server httpie pygments]);
in {
  options.valentino.modules.dev.python.enable = mkEnableOption "python language support and language server";

  config = mkIf cfg.enable {
    home.packages = [pyEnv];
  };
}
