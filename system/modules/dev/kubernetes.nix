{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.kubernetes;
in {
  options.system.modules.dev.kubernetes = {
    enable = mkEnableOption "Enable kubernetes capabilities";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      kubectl         # Kubernetes CLI
      kind            # Kubernetes IN Docker - local clusters for testing Kubernetes
      minikube        # A tool that makes it easy to run Kubernetes locally
      kubernetes-helm # Kubernetes package manager
      k9s             # Kubernetes CLI To Manage Your Clusters In Style
      kubectx         # Fast way to switch between clusters and namespaces in kubectl!
    ];
  };
}
