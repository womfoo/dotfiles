{
  lib,
  pkgs,
  config,
  secrets,
  ...
}:
let
  keycloakTlsPort = 48443;
  httpPort = config.services.nginx.defaultHTTPListenPort;
  # the host as seen from k3s pods (cni0)
  hostIP = "10.42.0.1";
  routePorts = [
    {
      name = "http";
      port = httpPort;
    }
    {
      name = "https";
      port = keycloakTlsPort;
    }
  ];
in
{

  security.acme.acceptTerms = true;

  imports = [
    secrets."keycloak-db-password".nixosModule
  ];

  services.nginx = {
    # enable = true;

    # # enable recommended settings
    # recommendedGzipSettings = true;
    # recommendedOptimisation = true;
    # recommendedTlsSettings = true;
    # recommendedProxySettings = true;

    virtualHosts = {
      "auth.kranium.net" = {
        forceSSL = true;
        enableACME = true;
        # 80/443 belong to k3s traefik, which forwards here via nixos-keycloak below
        listen = [
          {
            addr = "0.0.0.0";
            port = httpPort;
          }
          {
            addr = "0.0.0.0";
            port = keycloakTlsPort;
            ssl = true;
          }
        ];
        locations = {
          "/cloak/" = {
            proxyPass = "http://localhost:${toString config.services.keycloak.settings.http-port}/cloak/";
            recommendedProxySettings = true;
          };
        };
      };
    };
  };

  services.postgresql.enable = true;

  services.keycloak = {
    enable = true;

    database = {
      type = "postgresql";
      createLocally = true;

      username = "keycloak";
      passwordFile = secrets."keycloak-db-password".path config;
    };

    settings = {
      hostname = "auth.kranium.net";
      http-relative-path = "/cloak";
      http-port = 38080;
      # proxy = "passthrough";
      proxy-headers = "xforwarded";
      http-enabled = true;
    };
  };

  # k3s traefik: pass auth.kranium.net TLS through untouched to nginx on the host,
  # plain HTTP (ACME challenges, https redirect) goes to nginx too
  services.k3s.manifests.nixos-keycloak.content = [
    {
      apiVersion = "v1";
      kind = "Service";
      metadata = {
        name = "nixos-keycloak";
        namespace = "kube-system";
      };
      spec.ports = map (p: p // { protocol = "TCP"; }) routePorts;
    }
    {
      apiVersion = "discovery.k8s.io/v1";
      kind = "EndpointSlice";
      metadata = {
        name = "nixos-keycloak";
        namespace = "kube-system";
        labels."kubernetes.io/service-name" = "nixos-keycloak";
      };
      addressType = "IPv4";
      endpoints = [ { addresses = [ hostIP ]; } ];
      ports = map (p: p // { protocol = "TCP"; }) routePorts;
    }
    {
      apiVersion = "traefik.io/v1alpha1";
      kind = "IngressRouteTCP";
      metadata = {
        name = "nixos-keycloak";
        namespace = "kube-system";
      };
      spec = {
        entryPoints = [ "websecure" ];
        routes = [
          {
            match = "HostSNI(`auth.kranium.net`)";
            services = [
              {
                name = "nixos-keycloak";
                port = keycloakTlsPort;
              }
            ];
          }
        ];
        tls.passthrough = true;
      };
    }
    {
      apiVersion = "traefik.io/v1alpha1";
      kind = "IngressRoute";
      metadata = {
        name = "nixos-keycloak";
        namespace = "kube-system";
      };
      spec = {
        entryPoints = [ "web" ];
        routes = [
          {
            kind = "Rule";
            match = "Host(`auth.kranium.net`)";
            services = [
              {
                name = "nixos-keycloak";
                port = httpPort;
              }
            ];
          }
        ];
      };
    }
  ];

}
