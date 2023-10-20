{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.editor.ide;
in {
  options.system.modules.editor.ide = {
    enable = mkEnableOption "Enable ide capabilities";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # I have to start IntelliJ and other XWayland apps with GDK_SCALE-1 GDK_DPI_SCALE=0.5 idea.sh. Not as crisp as native Wayland applications but way better than before.
      # TODO: Enable it when needed
      # (androidStudioPackages.dev.override {tiling_wm = true;})
      jetbrains.idea-community
      vscodium
      postman
      httpie-desktop
    ];
  };
}
