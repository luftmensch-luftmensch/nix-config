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
    ]) ++ (optionals cfg.nix-packages [
    ]) ++ (optionals cfg.c-packages [
    ]) ++ (optionals cfg.go-packages [
      go gopls
    ]) ++ (optionals cfg.java-packages [
    ]) ++ (optionals cfg.misc-packages [
      direnv
      libtool
      mongodb-compass
    ]);
  }];
}
