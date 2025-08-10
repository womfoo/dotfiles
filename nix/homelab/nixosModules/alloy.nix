{
  lib,
  pkgs,
  config,
  ...
}:
{

  users.extraUsers.alloy = {
    isSystemUser = true;
    group = "nginx";
  };

  services.alloy.enable = true;

  systemd.services.alloy.serviceConfig.ReadOnlyPaths = "/var/log/nginx/";
  services.alloy.extraFlags = [
    "--server.http.listen-addr=127.0.0.1:12346"
    "--disable-reporting"
  ];

  environment.etc."alloy/prometheus.alloy".text = builtins.readFile ./prometheus.alloy;

  environment.etc."alloy/nginx.alloy".text = ''
    loki.write "grafana_localhost" {
    	endpoint {
    		url = "http://localhost:53100/loki/api/v1/push"
      }
    }

    loki.write "grafana_cloud_loki" {
    	endpoint {
    		url = "https://logs-prod-017.grafana.net/loki/api/v1/push"
    		basic_auth {
    			username = "855408"
    			password = "REDACTED"
    		}
    	}
    }
  '';

}
