{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.editor.ide;
in {
  options.system.modules.editor.ide = {
    enable = mkEnableOption "Enable ide capabilities (intellij-idea & vscode)";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      jetbrains.idea-community
      vscodium
      # postman
    ];
  };
}
