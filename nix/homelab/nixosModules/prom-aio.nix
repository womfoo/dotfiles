{
  lib,
  pkgs,
  config,
  ...
}:
let
  inherit (inputs.lihim.lihim.constants) d1 d2 tbe;
  items = d1 ++ d2 ++ tbe;
  items' = [
    "gikos.net" # nc
    "kranium.au" # cf
    "kranium.net" # cf
    "muinark.com" # gd
    "voxfront.com" # nc
    "streakycal.com" # cf
    "ralleta.com" # ss
  ];
in
{

  # FIXME: this is still painful with own hcl
  services.alloy.enable = true;
  environment.etc."alloy/prometheus.alloy".text = builtins.readFile ./prom-aio.alloy;
  # FIXME: ugly AF, figure out alloy foreach
  environment.etc."alloy/blackbox-targets.alloy".text = ''
    prometheus.exporter.blackbox "blackbox" {
      config  = "{ modules: { http_2xx: { prober: http, timeout: 5s } } }"
      targets = [
        ${lib.concatMapStringsSep "\n" (
          item: "{	\"name\" = \"${item}\", \"address\" = \"${item}\", \"module\" = \"http_2xx\", },"
        ) items}
      ]
    }
  '';

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
