{
  config,
  lib,
  ...
}: {
  ## System security tweaks
  # sets hidepid=2 on /proc (make process info visible only to owning user)
  security = {
    protectKernelImage = true; # Prevent replacing the running kernel w/o reboot

    # lockKernelModules = true; # Disable kernel module loading once the system is fully initialised

    # Make hardened profile more usable (This should be enabled alongside the hardened profile)
    # allowUserNamespaces = true;       # Must be set to true for `nix build` to function (https://nixos.org/manual/nixos/stable/index.html#sec-profile-hardened)
    # lockKernelModules = false;        # https://discourse.nixos.org/t/default-security-settings/9755

    # Sudo customization
    sudo = {
      wheelNeedsPassword = false;
      execWheelOnly = true;
    };
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
    kernel.sysctl = {
      # The Magic SysRq key is a key combo that allows users connected to the
      # system console of a Linux kernel to perform some low-level commands.
      # Disable it, since we don't need it, and is a potential security concern.
      "kernel.sysrq" = 0; # don't allow sysrqs (Magic SysRq is a key combination directly intercepted by the kernel and can be used, among other things, to perform an emergency shutdown)
      #"kernel.printk" = "3 4 3 3"; # don't let logging bleed into TTY

      # SHMMAX is the maximum size of a shared memory segment on a Linux system whereas
      # SHMALL is the maximum allocation of shared memory pages on a system
      #"kernel.shmmax" = 4294967296;
      #"kernel.shmall" = 4194304;

      ## TCP hardening
      # Prevent bogus ICMP errors from filling up logs.
      "net.ipv4.icmp_ignore_bogus_error_responses" = 1;

      # Ignore broadcast ICMP (mitigate SMURF)
      #"net.ipv4.icmp_echo_ignore_broadcasts" = 1; # Refuse ICMP echo requests on my desktop/laptop; nobody has any business  pinging them, unlike my servers.

      # Reverse path filtering causes the kernel to do source validation of
      # packets received from all interfaces. This can mitigate IP spoofing.
      "net.ipv4.conf.default.rp_filter" = 1;
      "net.ipv4.conf.all.rp_filter" = 1;
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

      # Log packets with impossible addresses to kernel log.log_martians
      # for the interface will be enabled if at least one of
      # conf/{all,interface}/log_martians is set to TRUE, it will be disabled otherwise
      #"net.ipv4.conf.default.log_martians" = 1;
      #"net.ipv4.conf.all.log_martians" = 1;

      #"net.ipv6.conf.all.use_tempaddr" = 2;

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

      # DDoS Protection With IPtables - Source: https://javapipe.com/blog/iptables-ddos-protection/

      #"net.core.netdev_max_backlog" = 262144; # Increase number of incoming connections backlog
      #"net.core.rmem_default" = 31457280; # Default Socket Receive Buffer
      #"net.core.rmem_max" = 67108864; # Maximum Socket Receive Buffer
      #"net.core.wmem_default" = 31457280; # Default Socket Send Buffer
      #"net.core.wmem_max" = 67108864; # Maximum Socket Send Buffer
      #"net.core.somaxconn" = 65535; # Increase number of incoming connections
      #"net.core.optmem_max" = 25165824; # Increase the maximum amount of option memory buffers

      #"vm.swappiness" = 0; # or 10

      # Intel GPU Analysis
      #"dev.i915.perf_stream_paranoid" = 0;
    };
  };
}
