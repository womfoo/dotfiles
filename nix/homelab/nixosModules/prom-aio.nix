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
    "https://gikos.net" # nc
    "https://kranium.au" # cf
    "https://kranium.net" # cf
    "https://muinark.com" # gd
    "https://ralleta.com" # ss
  ];

  blackboxConfigFile = pkgs.writers.writeYAML "blackbox.yaml" {

    modules = {
      http_2xx = {
        prober = "http";
        timeout = "100s";
        http = {
          fail_if_ssl = false;
          fail_if_not_ssl = false;
          valid_status_codes = [
            200
            301
            302
            404
            525
            526
          ];
          preferred_ip_protocol = "ip4";
          ip_protocol_fallback = false;
          follow_redirects = false;
          tls_config.insecure_skip_verify = true;
        };
      };
    };

  };

in
{

  # FIXME: this is still painful with own hcl
  services.alloy.enable = true;
  environment.etc."alloy/prometheus.alloy".text = builtins.readFile ./prom-aio.alloy;
  # FIXME: ugly AF, figure out alloy foreach
  environment.etc."alloy/blackbox-targets.alloy".text = ''
    prometheus.exporter.blackbox "blackbox" {
      config_file = "${blackboxConfigFile}"
      targets = [
        ${lib.concatMapStringsSep "\n" (
          item: "{ \"name\" = \"${item}\", \"address\" = \"${item}\", \"module\" = \"http_2xx\", },"
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
