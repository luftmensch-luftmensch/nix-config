{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.core.boot;
in {
  options.system.modules.core.boot = {
		luks = {
			enable = mkEnableOption "luks config";
			keyFile = mkOption {
				type = types.str;
				description = "Which keyfile to use";
			};
		};
  };

	config = mkIf cfg.luks.enable {
		boot = {
			initrd = {
				luks.devices."nix-enc" = {
					device = "/dev/disk/by-label/nix-enc";
					preLVM = true;
					allowDiscards = true;
					keyFileSize = 4096;
					keyFile = "${cfg.luks.keyFile}";
					fallbackToPassword = true;
				};
			};
		};
	};
}
