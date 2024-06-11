{
  config,
  lib,
  pkgs,
  ...
}:
with lib builtins;
let
  cfg = config.valentino.modules.dev.generics;

  apply-hm-env = pkgs.writeShellScript "apply-hm-env" ''
    ${optionalString (config.home.sessionPath != [ ]) ''
      export PATH=${concatStringsSep ":" config.home.sessionPath}:$PATH
    ''}
    ${concatStringsSep "\n" (
      lib.mapAttrsToList (k: v: ''
        export ${toString k}=${toString v}
      '') config.home.sessionVariables
    )}
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

  runbg = pkgs.writeShellScriptBin "runbg" ''
    [ $# -eq 0 ] && {
        echo "$(basename "$0"): missing command" >&2
        exit 1
    }
    prog="$(which "$1")"
    [ -z "$prog" ] && {
        echo "$(basename "$0"): unknown command: $1" >&2
        exit 1
    }
    shift  # remove $1, now $prog, from args
    tty -s && exec </dev/null      # if stdin is a terminal, redirect from null
    tty -s <&1 && exec >/dev/null  # if stdout is a terminal, redirect to null
    tty -s <&2 && exec 2>&1        # stderr to stdout (which might not be null)
    "$prog" "$@" &
  '';
in
{
  options.valentino.modules.dev.generics.enable = mkEnableOption "uncategorized packages for programming";

  config = mkIf cfg.enable {
    home.packages = [
      run-as-service
      runbg
    ];
  };
}
