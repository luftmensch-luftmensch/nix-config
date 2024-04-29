{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.kube;
in {
  options.valentino.modules.dev.kube.enable = mkEnableOption "kubernetes tools";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      kubectl
      kind
      minikube
      kubernetes-helm
      k9s
      kubectx
    ];
  };
}
