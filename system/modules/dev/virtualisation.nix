{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.virtualisation;
in {
  options.system.modules.dev.virtualisation.enable = mkEnableOption "Enable virtualisation w/ libvirt";

  config = mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        ovmf = {
          enable = true;
          packages = [pkgs.OVMFFull];
        };
        swtpm.enable = true;
      };
    };

    environment.systemPackages = with pkgs; [virt-manager virt-viewer swtpm];
  };
}
