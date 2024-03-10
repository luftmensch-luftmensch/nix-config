{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.python;
in {
  options.valentino.modules.dev.python = {
    enable = mkEnableOption "python language support and language server";
  };

  config = mkIf cfg.enable {
    home.packages =
      [pkgs.python311]
      ++ (with pkgs.python311Packages; [
        flake8
        python-lsp-server
        httpie
        pygments
      ]);
  };
}
