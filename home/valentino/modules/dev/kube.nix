{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.kube;
in {
  options.valentino.modules.dev.kube = {
    enable = mkEnableOption "kubernetes tools";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      kubectl         # Kubernetes CLI
      kind            # Kubernetes IN Docker - local clusters for testing Kubernetes
      minikube        # A tool that makes it easy to run Kubernetes locally
      kubernetes-helm # Kubernetes package manager
      k9s             # Kubernetes CLI To Manage Your Clusters In Style
      kubectx         # Fast way to switch between clusters and namespaces in kubectl!
    ];
  };
}
