{ config, lib, ... }:
{
  ## System security tweaks
  security = {
    protectKernelImage = true; # Prevent replacing the running kernel w/o reboot

    # lockKernelModules = true; # Disable kernel module loading once the system is fully initialised

    # Make hardened profile more usable
    # allowUserNamespaces = true;       # Must be set to true for `nix build` to function (https://nixos.org/manual/nixos/stable/index.html#sec-profile-hardened)
    # lockKernelModules = false;        # https://discourse.nixos.org/t/default-security-settings/9755

    # Sudo customization
    sudo = {
      wheelNeedsPassword = false;
      execWheelOnly = true;
    };

    # sudo-rs = {
    #   enable = true;
    #   execWheelOnly = true;
    # };

    # Whether to enable the RealtimeKit system service, which hands out realtime scheduling priority to user processes on demand.
    # For example, the PulseAudio server uses this to acquire realtime priority.
    # Recommended for pipewire
    rtkit.enable = true;

    # So we don't have to do this later...
    acme.acceptTerms = true;
  };

  boot = {
    # tmpfs = /tmp is mounted in ram. Doing so makes temp file management speedy
    # on ssd systems, and volatile! Because it's wiped on reboot.
    tmp = {
      useTmpfs = true;

      # If not using tmpfs, which is naturally purged on reboot, we must clean it
      # /tmp ourselves. /tmp should be volatile storage!
      cleanOnBoot = lib.mkDefault (!config.boot.tmp.useTmpfs);
    };

    # https://github.com/NixOS/nixpkgs/blob/d6fe32c6b9059a054ca0cda9a2bb99753d1134df/nixos/modules/profiles/hardened.nix#L95
    # https://madaidans-insecurities.github.io/guides/linux-hardening.html#sysctl
    kernel.sysctl = {
      "kernel.kptr_restrict" = 2;
      "kernel.sysrq" = 0;
      "kernel.printk" = "3 3 3 3"; # don't let logging bleed into TTY
      "kernel.unprivileged_bpf_disabled" = 1;
      "net.core.bpf_jit_harden" = 2;
      "vm.unprivileged_userfaultfd" = 0;
      "kernel.kexec_load_disabled" = 1;

      ## TCP hardening
      # Prevent bogus ICMP errors from filling up logs.
      "net.ipv4.icmp_ignore_bogus_error_responses" = 1;

      # Reverse path filtering causes the kernel to do source validation of
      # packets received from all interfaces. This can mitigate IP spoofing.
      "net.ipv4.conf.all.rp_filter" = 1;
      "net.ipv4.conf.default.rp_filter" = 1;

      # Do not accept IP source route packets (we're not a router)
      "net.ipv4.conf.all.accept_source_route" = 0;
      "net.ipv6.conf.all.accept_source_route" = 0;
      # Don't send ICMP redirects (again, we're on a router)
      "net.ipv4.conf.all.send_redirects" = 0;
      "net.ipv4.conf.default.send_redirects" = 0;
      # Refuse ICMP redirects (MITM mitigations)
      "net.ipv4.conf.all.accept_redirects" = 0;
      "net.ipv4.conf.default.accept_redirects" = 0;
      "net.ipv4.conf.all.secure_redirects" = 0;
      "net.ipv4.conf.default.secure_redirects" = 0;
      "net.ipv6.conf.all.accept_redirects" = 0;
      "net.ipv6.conf.default.accept_redirects" = 0;

      "net.ipv4.conf.default.accept_source_route" = 0;
      "net.ipv6.conf.default.accept_source_route" = 0;

      # Malicious IPv6 router advertisements can result in a man-in-the-middle attack, so they should be disabled.
      "net.ipv6.conf.all.accept_ra" = 0;
      "net.ipv6.conf.default.accept_ra" = 0;

      # Protects against SYN flood attacks
      "net.ipv4.tcp_syncookies" = 1;
      # Incomplete protection again TIME-WAIT assassination
      "net.ipv4.tcp_rfc1337" = 1;

      ## TCP optimization
      # TCP Fast Open is a TCP extension that reduces network latency by packing
      # data in the sender’s initial TCP SYN. Setting 3 = enable TCP Fast Open for
      # both incoming and outgoing connections:
      "net.ipv4.tcp_fastopen" = 3;
      # Bufferbloat mitigations + slight improvement in throughput & latency
      "net.ipv4.tcp_congestion_control" = "bbr"; # htcp
      "net.core.default_qdisc" = "cake"; # fq

      #"vm.swappiness" = 0; # or 10
    };

    # TODO: Try out
    # blacklistedKernelModules = [
    #   # Obscure network protocols
    #   "ax25"
    #   "dccp"
    #   "netrom"
    #   "rds"
    #   "rose"
    #   "stcp"
    #   "tipc"
    #   # Old or rare or insufficiently audited filesystems
    #   "adfs"
    #   "affs"
    #   "bfs"
    #   "befs"
    #   "cramfs"
    #   "efs"
    #   "erofs"
    #   "exofs"
    #   "freevxfs"
    #   "f2fs"
    #   "hfs"
    #   "hpfs"
    #   "jfs"
    #   "minix"
    #   "nilfs2"
    #   "ntfs"
    #   "omfs"
    #   "qnx4"
    #   "qnx6"
    #   "sysv"
    #   "ufs"
    # ];
  };
}
