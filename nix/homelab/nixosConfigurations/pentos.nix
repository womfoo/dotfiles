{
  pkgs,
  config,
  ...
}:
{
  bee.system = "x86_64-linux";
  bee.pkgs = import inputs.nixos-24-11 {
    inherit (inputs.nixpkgs) system;
    allowUnfree = true;
  };
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  imports = [
    (inputs.nixpkgs + /nixos/modules/virtualisation/oci-image.nix)
    cell.nixosModules.common
    cell.nixosModules.gikos-kranium
    cell.secrets."attic-env".nixosModule
    inputs.srvos.nixosModules.mixins-nginx
    inputs.srvos.nixosModules.mixins-telegraf
    inputs.srvos.nixosModules.server
  ];

  # networking.firewall.interfaces.enp0s31f6.allowedUDPPorts = [ 51820 ];
  # networking.firewall.interfaces.enp0s20f0u3u2.allowedTCPPorts = [ 9090 ];
  networking.hostName = "pentos";
  users.mutableUsers = pkgs.lib.mkForce true;
  services.atticd = {
    enable = true;
    environmentFile = cell.secrets."attic-env".path config;
    settings = {
      listen = "127.0.0.1:8080";
      jwt = { };
    };
  };

  services.tmate-ssh-server.enable = true;

  environment.systemPackages = with pkgs; [ attic-client ];
  security.acme.acceptTerms = true;
  security.acme.defaults.email = "kranium@gikos.net";
  services.nginx.virtualHosts = {
    "cache.kranium.au" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8080/";
        extraConfig = ''
          client_max_body_size 1G;
          set $whitelist 0;

          if ($http_user_agent ~* "curl/.*Nix") {
              set $whitelist 1;
          }
          if ($http_user_agent ~* "Attic/") {
              set $whitelist 1;
          }
          if ($whitelist = 1) {
              return 200;
          }
          return 403;
        '';
      };
    };
  };
}
