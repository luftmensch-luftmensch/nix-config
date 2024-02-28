{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with builtins; let
  cfg = config.valentino.modules.dev.generics;

  apply-hm-env = pkgs.writeShellScript "apply-hm-env" ''
    ${optionalString (config.home.sessionPath != []) ''
      export PATH=${concatStringsSep ":" config.home.sessionPath}:$PATH
    ''}
    ${concatStringsSep "\n" (lib.mapAttrsToList (k: v: ''
        export ${toString k}=${toString v}
      '')
      config.home.sessionVariables)}
    ${config.home.sessionVariablesExtra}
    exec "$@"
  '';
  # runs processes as systemd transient services
  run-as-service = pkgs.writeShellScriptBin "run-as-service" ''
    exec ${pkgs.systemd}/bin/systemd-run \
      --slice=app-manual.slice \
      --property=ExitType=cgroup \
      --user \
      --wait \
      bash -lc "exec ${apply-hm-env} $@"
  '';
in {
  options.valentino.modules.dev.generics = {
    enable = mkEnableOption "uncategorized packages for programming";
  };

  config = mkIf cfg.enable {
    home.packages = [run-as-service];
  };
}
