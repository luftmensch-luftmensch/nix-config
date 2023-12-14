{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.cc;
in {
  options.valentino.modules.dev.cc = {
    enable = mkEnableOption "c language support and language server";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      clang
      clang-tools
      bear
      ccls
      cmake
      cmake-language-server
      valgrind
      gdb
      gef
      # gcc
      gnumake
    ];
  };
}
