{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.xorg;
in {
  options.valentino.modules.xorg = {
    enable = mkEnableOption "xorg configuration management for user";

    wm = mkOption {
      description = "An option to choose the window manager [xorg] configuration to enable";
      default = null;
      type = types.nullOr (types.enum ["i3"]);
      example = "i3";
    };
  };

  config = mkIf cfg.enable {
    xsession.enable = true;

    home.packages = with pkgs; [
      parcellite
      scrot
      xclip
      xdotool

      xautolock # Fire  up programs  in case  of user  inactivity under X
      xclip # Command line interface to X selections (clipboard)
      xorg.xev # Print contents of X events
      xorg.xkill # Kill a client by its X resource
      xorg.xrdb # X server resource database utility
      xss-lock # Use external locker as X screen saver
      xfce.xfce4-screenshooter # Screenshoter utility
    ];

    home.file.".config/parcellite/parcelliterc" = {
      enable = true;
      text = ''
        [rc]
        RCVersion=1
        use_copy=true
        use_primary=true
        synchronize=true
        save_history=true
        history_pos=false
        history_x=1
        history_y=1
        history_limit=25
        data_size=0
        item_size=5
        automatic_paste=false
        auto_key=false
        auto_mouse=true
        key_input=false
        restore_empty=true
        rc_edit=false
        type_search=false
        case_search=false
        ignore_whiteonly=true
        trim_wspace_begend=false
        trim_newline=false
        hyperlinks_only=false
        confirm_clear=false
        current_on_top=true
        single_line=true
        reverse_history=false
        item_length=50
        persistent_history=false
        persistent_separate=false
        persistent_on_top=false
        persistent_delim=\\n
        nonprint_disp=false
        ellipsize=2
        multi_user=true
        icon_name=parcellite
        history_key=<Ctrl><Alt>H
      '';
    };

    valentino.modules.xorg = {
      locker.enable = true;
      picom.enable = true;
    };
  };
}
