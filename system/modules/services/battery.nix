{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.services.battery;
in {
  options.system.modules.services.battery = {
    enable = mkEnableOption "Enable TLP capabilities";
  };

  config = mkIf cfg.enable {
    # TLP settings: https://linrunner.de/tlp/settings/index.html
    services.tlp = {
      enable = true;
      settings = {
        # CHARGE THRESHOLD - BAT #

        #"START_CHARGE_THRESH_BAT0" = 0;
        #"STOP_CHARGE_THRESH_BAT0" = 0;

        #"START_CHARGE_THRESH_BAT1" = 0;
        #"STOP_CHARGE_THRESH_BAT1" = 0;

        # AHCI OPTIONS #
        #AHCI_RUNTIME_PM_TIMEOUT=15;

        # BAY OPTIONS #
        #BAY_POWEROFF_ON_AC=0;
        #BAY_POWEROFF_ON_BAT=0;
        #BAY_DEVICE="sr0";

        # CPU - AC #
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_MAX_PERF_ON_AC = 70;
        #CPU_MIN_PERF_ON_AC = 0;
        #CPU_BOOST_ON_AC = 1;
        #CPU_ENERGY_PERF_POLICY_ON_AC="performance";
        #CPU_HWP_ON_AC = "performance";
        #CPU_HWP_DYN_BOOST_ON_AC=1;

        # CPU - BAT #
        CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";
        CPU_MAX_PERF_ON_BAT = 60;
        #CPU_MIN_PERF_ON_BAT = 0;
        #CPU_BOOST_ON_BAT = 1;
        #CPU_ENERGY_PERF_POLICY_ON_BAT="power";
        #CPU_HWP_ON_BAT = "performance";
        #CPU_HWP_DYN_BOOST_ON_BAT=0;

        # DISK OPTIONS #

        #DISK_IDLE_SECS_ON_AC=0;
        #DISK_IDLE_SECS_ON_BAT=2;
        #DISK_DEVICES="sda sdb";
        #DISK_APM_LEVEL_ON_AC="254 254";
        #DISK_APM_LEVEL_ON_BAT="128 128";

        #"DISK_IOSCHED" = "mq-deadline mq-deadline";

        # DEVICES #
        #DEVICES_TO_ENABLE_ON_STARTUP = "bluetooth wifi";
        #RESTORE_DEVICE_STATE_ON_STARTUP = 0;

        # USB OPTIONS #
        #USB_AUTOSUSPEND = 0;
        #USB_BLACKLIST_BTUSB=0;
        #USB_BLACKLIST_PHONE=0;
        #USB_BLACKLIST_PRINTER=1;
        #USB_BLACKLIST_WWAN=1;

        # ENERGY POLICY - AC #
        #ENERGY_PERF_POLICY_ON_AC = "performance";

        # ENERGY POLICY - BAT #
        #ENERGY_PERF_POLICY_ON_BAT = "power";

        # MODES #
        #TLP_DEFAULT_MODE = "AC";
        #TLP_PERSISTENT_DEFAULT=0;

        # MOST LOST WORK #
        #"MAX_LOST_WORK_SECS_ON_AC" = 15;
        #"MAX_LOST_WORK_SECS_ON_BAT" = 60;

        # NMI #
        #"NMI_WATCHDOG" = 0;

        # PLATFORM - PROFILE #
        #PLATFORM_PROFILE_ON_AC="performance";
        #PLATFORM_PROFILE_ON_BAT="low-power";

        # PCIE  - AC #
        #PCIE_ASPM_ON_AC = default;

        # PCIE  - BAT #
        #PCIE_ASPM_ON_BAT = default;

        # RADEON OPTIONS #
        #RADEON_POWER_PROFILE_ON_AC=high;
        #RADEON_POWER_PROFILE_ON_BAT=low;
        #RADEON_DPM_STATE_ON_AC=performance;
        #RADEON_DPM_STATE_ON_BAT=battery;
        #RADEON_DPM_PERF_LEVEL_ON_AC=auto;
        #RADEON_DPM_PERF_LEVEL_ON_BAT=auto;

        # RUNTIME PM - AC #
        #RUNTIME_PM_ON_AC = "on";

        # RUNTIME PM - BAT #
        #RUNTIME_PM_ON_BAT = "auto";

        # SATA DISK #
        #"SATA_LINKPWR_ON_AC" = "med_power_with_dipm max_performance";
        #"SATA_LINKPWR_ON_BAT" = "min_power min_power med_power_with_dipm";

        # SCHED - AC #
        #SCHED_POWERSAVE_ON_AC = 0;

        # SCHED - BAT #
        #SCHED_POWERSAVE_ON_BAT = 1;

        #     SOUND      #
        #"SOUND_POWER_SAVE_CONTROLLER" = "Y";

        # SOUND - AC #
        #SOUND_POWER_SAVE_ON_AC = 0;

        # SOUND - BAT #
        #SOUND_POWER_SAVE_ON_BAT = 1;

        # WAKE ON LAN #
        #"WOL_DISABLE" = "Y";

        # WIFI - AC #
        #WIFI_PWR_ON_AC = "off";

        # WIFI - BAT #
        #WIFI_PWR_ON_BAT = "on";
      };
    };
  };
}
