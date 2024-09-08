{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.system.modules.hardware.audio;
in
{
  options.system.modules.hardware.audio = {
    enable = mkEnableOption "Enable audio capabilities";
    pipewire.enable = mkEnableOption "Enable audio capabilities w/ pipewire";
    pulseaudio.enable = mkEnableOption "Enable audio capabilities w/ pulseaudio";
  };

  config = mkIf cfg.enable (mkMerge [
    { sound.enable = true; }

    (mkIf cfg.pipewire.enable {
      # To solve the error showed in journalctl (pipewire-pulse[1413]: execvp error 'pactl': No such file or directory)
      # Use: systemd.user.services.pipewire-pulse.path = [ pkgs.pulseaudio ];
      # systemctl --user restart pipewire.service (Taken from https://www.reddit.com/r/pop_os/comments/v3g2w9/is_there_a_cli_command_to_restart_pipewire/)
      services.pipewire = {
        # In order to use it you need to set hardware.pulseaudio.enable = false
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        jack.enable = true;
        pulse.enable = true;
        # lowLatency.enable = true;
      };
    })

    (mkIf cfg.pulseaudio.enable { hardware.pulseaudio.enable = true; })
  ]);
}
