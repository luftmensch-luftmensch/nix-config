{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.programming;
in {
  options.system.modules.packages.programming = {
    python-packages = mkEnableOption "Enable python packages";
    nix-packages    = mkEnableOption "Enable nix packages";
    c-packages      = mkEnableOption "Enable C/C++ packages";
    java-packages   = mkEnableOption "Enable java packages";
    go-packages     = mkEnableOption "Enable golang packages";
    misc-packages   = mkEnableOption "Enable other language packages";
  };

  config = mkMerge [{
    environment.systemPackages = with pkgs; [
      
    ] ++ (optionals cfg.python-packages [
      python311
      python311Packages.mutagen           # Used to add information to music
      python311Packages.pip               # Python pm
      python311Packages.psutil            # Process and system utilization information interface
      python311Packages.setuptools        # Utilities to facilitate the installation of Python packages
      python311Packages.flake8            # python tool that glues together pycodestyle, pyflakes, mccabe, and third-party plugins to check the style and quality of some python code. 
      python311Packages.python-lsp-server # python language server
      python311Packages.httpie            # Command line http client (API Testing)
      python311Packages.pygments          # Used for minted (syntax highlighting)
    ]) ++ (optionals cfg.nix-packages [
      deadnix                             # Scan Nix files for dead code
      nix-index                           # Locate packages
      nix-prefetch-git                    # Same logic of nix-prefetch-url
      nix-output-monitor                  # Additional info while building (more info at https://github.com/maralorn/nix-output-monitor)
      # niv                               # Dependency management for Nix
      # patchelf                          # Patches binaries for Nix support
    ]) ++ (optionals cfg.c-packages [
      clang_16 clang-tools ccls
      bear                                # Tool that generates a compilation database for clang tooling
      gcc
      gdb valgrind gef
      cmake gnumake
      unixtools.xxd                       # Make a hexdump or do the reverse (xxd -i <file>)
    ]) ++ (optionals cfg.go-packages [
      go gopls
    ]) ++ (optionals cfg.java-packages [
      jdk
      maven gradle
    ]) ++ (optionals cfg.misc-packages [
      direnv
      libtool
      nodejs yarn
      shellcheck
      mongodb-compass
    ]);
  }];
}
