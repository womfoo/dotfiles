{ pkgs, lib, ... }:
{
  systemd.services.heartbeat = {
    enable = true;
    description = "Enable heartbeat & network activity led on Helios64";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "helios64-leds.sh" ''
        echo heartbeat | ${lib.getBin pkgs.coreutils}/bin/tee /sys/class/leds/helios64::status/trigger
        echo netdev | ${lib.getBin pkgs.coreutils}/bin/tee /sys/class/leds/helios64:blue:net/trigger
        echo eth0 | ${lib.getBin pkgs.coreutils}/bin/tee /sys/class/leds/helios64:blue:net/device_name
        echo 1 | ${lib.getBin pkgs.coreutils}/bin/tee /sys/class/leds/helios64:blue:net/link
        echo 1 | ${lib.getBin pkgs.coreutils}/bin/tee /sys/class/leds/helios64:blue:net/rx
        echo 1 | ${lib.getBin pkgs.coreutils}/bin/tee /sys/class/leds/helios64:blue:net/tx
      '';
    };
    after = [ "getty.target" ];
    wantedBy = [ "multi-user.target" ];
  };
}
