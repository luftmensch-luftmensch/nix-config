{
  options,
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
    home.packages = with pkgs; [
      python311
      python311Packages.python-lsp-server
      python311Packages.mutagen           # Used to add information to music
      python311Packages.pip               # Python pm
      python311Packages.psutil            # Process and system utilization information interface
      python311Packages.setuptools        # Utilities to facilitate the installation of Python packages
      python311Packages.flake8            # python tool that glues together pycodestyle, pyflakes, mccabe, a
      python311Packages.python-lsp-server # python language server
      python311Packages.httpie            # Command line http client (API Testing)
      python311Packages.pygments  
    ];
  };
}

