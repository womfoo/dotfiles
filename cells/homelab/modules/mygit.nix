{ config, lib, pkgs, ... }:
with lib;
{
  options = {
    services.mygit = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
      lan = mkOption {
        type = types.str;
        default = "boo";
        example = "boo";
      };
    };

  };
  config =
    let cfg_ = config.services.mygit;
        cfg = config.services.sourcehut;
    in mkIf cfg_.enable {
      # boot.isContainer = true;
      networking.firewall.allowedTCPPorts = [
        config.services.nginx.defaultHTTPListenPort
      ];
      networking.hosts = {
        "127.0.0.1" = [
          "builds.sr.ht.local"
          "git.sr.ht.local"
          "hub.sr.ht.local"
          "logs.sr.ht.local"
          "man.sr.ht.local"
          "meta.sr.ht.local"
          "sr.ht.local"
        ];
      };
      services.sourcehut = {
        enable = true;
        originBase = "sr.ht.local";
        services = [
          "builds"
          "git"
          "hub"
          "man"
          "meta"
        ];
        builds = {
          enable = true;
          enableWorker = true;
        };
        settings = {
          "sr.ht" = {
            environment = "production";
            global-domain = "sr.ht.local";
            origin = "http://sr.ht.local";
            # nix shell nixpkgs#sourcehut.coresrht -c srht-keygen network
            network-key = "OeXzQ6A8Vcgt5QJkXScuxeXCtfdKzKev99BRNb3_CWQ=";
            # nix shell nixpkgs#sourcehut.coresrht -c srht-keygen service
            service-key = "62427596fed00fa48c19f95bc85c14d0c618a5f8c130b53ba9a6a6b403bf1507";
          };
          # nix shell nixpkgs#sourcehut.metasrht -c metasrht-manageuser -t admin -e mymail@gmail.com misuzu
          "meta.sr.ht" = {
            origin = "http://meta.sr.ht.local";
          };
          "builds.sr.ht" = {
            origin = "http://builds.sr.ht.local";
            oauth-client-secret = "8f5fc39b5948907e62c737f6b48462dc";
            oauth-client-id = "299db9f9c2013170";
          };
          "meta.sr.ht::settings" = {
            onboarding-redirect = "http://meta.sr.ht.local";
          };
          # nix shell nixpkgs#sourcehut.coresrht -c srht-keygen webhook
          webhooks.private-key= "U7yd/8mGs/v0O3kId4jpeSghUCa9tqP1fYQwSV8UOqo=";
        };
      };

      services.nginx = {
        enable = true;
        # only recommendedProxySettings are strictly required, but the rest make sense as well.
        recommendedTlsSettings = false;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;

        virtualHosts = with pkgs.lib; {
          "builds.sr.ht.local".forceSSL = mkForce false;
          "git.sr.ht.local".forceSSL = mkForce false;
          "hub.sr.ht.local".forceSSL = mkForce false;
          "logs.sr.ht.local".forceSSL = mkForce false;
          "man.sr.ht.local".forceSSL = mkForce false;
          "meta.sr.ht.local".forceSSL = mkForce false;
          "sr.ht.local".forceSSL = mkForce false;
        };
      };
    };
}
