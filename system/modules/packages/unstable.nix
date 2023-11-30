{
  lib,
  config,
  # inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.unstable;
  # unstable-pkgs = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;
in {
  options.system.modules.packages.unstable = {
    enable = mkEnableOption "Enable packages pulled from unstable branch";
    # onWayland = mkEnableOption "[Wayland only] Enable packages pulled from unstable branch";
  };

	config = mkIf cfg.enable {
		# environment.systemPackages = with unstable-pkgs; [
		# ];
	};

  # config = mkIf cfg.enable (
  #   mkMerge [
  #     {
  #       # environment.systemPackages = with unstable-pkgs; [
  #       # ];
  #     }
  #     (mkIf cfg.onWayland {
  #       environment.systemPackages = with unstable-pkgs; [
  #         swaynotificationcenter # Simple notification daemon with a GUI built for Sway
  #       ];
  #     })
  #   ]
  # );
}
