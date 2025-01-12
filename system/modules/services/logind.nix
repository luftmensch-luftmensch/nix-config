{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.system.modules.services.flatpak;
in
{
  options.system.modules.services.flatpak = {
    enable = mkEnableOption "Enable flatpak capabilities";
    pkgs = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [
        "org.mozilla.firefox"
        "org.mozilla.thunderbird"
      ];
      description = "A list of packages to install via flatpak";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.lenght cfg.pkgs > 0;
        message = "The pkgs option for flatpak must me specified";
      }
    ];

    services.flatpak.enable = true;

    # Stolen from https://www.reddit.com/r/NixOS/comments/1hzgxns/fully_declarative_flatpak_management_on_nixos/
    system.activationScripts.flatpakManagement = let
      _fk = "${pkgs.flatpak}/bin/flatpak";
      _gr = "${grep}/bin/grep";
    in {
      text = ''
      # Ensure the Flathub repo is added
       ${_fk} remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

      # Get currently installed Flatpaks
      installedFlatpaks=$(${_fk}list --app --columns=application)

      # Remove any Flatpaks that are NOT in the desired list
      for installed in $installedFlatpaks; do
        if ! echo ${toString cfg.pkgs} | ${_gr} -q $installed; then
          echo "Removing $installed because it's not in the flatpak pkgs list."
          ${_fk} uninstall -y --noninteractive $installed
        fi
      done

      # Install or re-install the Flatpaks you DO want
      for app in ${toString cfg.pkgs}; do
        echo "Ensuring $app is installed."
        ${_fk} install -y flathub $app
      done

      # Update all installed Flatpaks
      ${_fk} update -y
    '';
    };
  };
}
