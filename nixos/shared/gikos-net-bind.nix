{ config, lib, pkgs, ... }:
let
  responsePolicy = pkgs.writeText "rpz.db" ''
    $TTL 1H
    @                       SOA LOCALHOST. named-mgr.example.com (1 1h 15m 30d 2h)
                            NS  LOCALHOST.
    ; QNAME policy records.  There are no periods (.) after the owner names.
    ; nxdomain.domain.com     CNAME   .               ; NXDOMAIN policy
    ; rc.gikos.net            A   172.19.86.1
    ; habilog.gikos.net       A   172.19.86.1
  '';
  gikosNetZone = pkgs.writeText "gikos.net.db" ''
    ;
    ; dns zone for for gikos.net
    ;
    $ORIGIN gikos.net.
    $TTL 1D
    ; any time you make a change to the domain, bump the
    ; "serial" setting below. the format is easy:
    ; YYYYMMDDHH, with the HH being an iterator in case you
    ; make more than one change during any one day
    @     IN SOA   habilog hostmaster (
                        2022020323; serial
                        8H        ; refresh
                        4H        ; retry
                        4W        ; expire
                        1D )      ; minimum
    ; torvalds.schroder.net serves this domain as both the
    ; name server (NS) and mail exchange (MX)
                NS      habilog
    ;            MX      10 habilog
    ; define domain functions with CNAMEs
    rc           CNAME   habilog
    paperless    CNAME   habilog
    octoprint    CNAME   habilog
    www          CNAME   au01
    ; just in case someone asks for localhost.gikos.net
    ;localhost       A       127.0.0.1
    ; our hostnames, in alphabetical order
    habilog        A       172.19.86.1
    au01           A       149.28.180.24
    ; FIXME: make @ a CNAME to au01
    @              A       149.28.180.24
 '';

in
{
  services.bind = {
    enable = true;
    ipv4Only = true;
    cacheNetworks = [
      "127.0.0.0/24"
      "172.19.86.0/24"
      "172.19.87.0/24"
      ];
    extraOptions = ''
      response-policy { zone "rpz.db"; };
      querylog yes;
      recursion yes;
      dnssec-validation auto;
    '';
    extraConfig = ''
      statistics-channels {
      inet 127.0.0.1 port 8053;
      };
    '';
    zones = {
      "rpz.db" = {
        file = "${responsePolicy}";
        master = true;
      };
      "gikos.net" = {
        file = "${gikosNetZone}";
        master = true;
      };
    };
  };
}
