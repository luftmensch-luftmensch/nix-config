{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.valentino.modules.dev.cc;
in {
  options.valentino.modules.dev.cc = {
    enable = mkEnableOption "c language support and language server";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      clang
      clang-tools
      bear
      ccls
      cmake
      cmake-language-server
      valgrind
      gdb
      gef
      # gcc
      gnumake
    ];

    # Configuration specific for gdb
    #
    home.file.".config/gdb/gdbinit" = {
      enable = true;
      text = ''
        # To toggle ui : <C-x a>
        set confirm off
        set verbose off

        set prompt \033[31mgdb $ \033[0m
        set print pretty on
        set print thread-events off
        set history save on
        #set detach-on-fork off
        set listsize 10

        set disassembly-flavor intel

        # These make gdb never pause in its output
        set height 0
        set width 0

        set auto-load safe-path /
        set breakpoint pending on

        define src
          layout src
        end

        define btc
          bt
          continue
        end

        define loop-stepi
          while (1)
            stepi
          end
        end

        define loop-bt
          while (1)
            bt
            continue
          end
        end

        define segfaultaddr
          p $_siginfo._sifields._sigfault.si_addr
        end

        define lines
          if $argc < 0
            print "USAGE: lines ADDR1 [ADDR2...]"
            return
          end
          set $i = 0
          set $sum = 0
          while $i < $argc
            eval "info line *$arg%d", $i
            set $i = $i + 1
          end
          print $sum
        end

        macro define offsetof(t, f) &((t *) 0)->f

        ## Skip over the implementation's stack frames on abort/etc.
        if !$_isvoid($_any_caller_matches)
            define hook-stop
                while $_thread && $_any_caller_matches("^__|abort|raise")
                    up-silently
                end
            end
        end
      '';
    };
  };
}
