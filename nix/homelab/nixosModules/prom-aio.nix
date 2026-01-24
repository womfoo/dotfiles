{
  lib,
  pkgs,
  config,
  ...
}:
{

  # FIXME: this is still painful with own hcl
  services.alloy.enable = true;
  environment.etc."alloy/prometheus.alloy".text = builtins.readFile ./prom-aio.alloy;

  services.prometheus.exporters.node = {
    enable = true;
    port = 9100;
    enabledCollectors = [
      "logind"
      "systemd"
    ];
    # disabledCollectors = [ "textfile" ];
    openFirewall = true;
    firewallFilter = "-i br0 -p tcp -m tcp --dport 9100";
  };

  services.prometheus.enable = true;
  services.prometheus.extraFlags = [
    "--web.enable-remote-write-receiver"
    "--storage.tsdb.retention.time=360d"
  ];
}
