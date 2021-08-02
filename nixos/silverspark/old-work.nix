{ pkgs, ... }:
{
  # nixpkgs.config.overlays = [ "/home/kranium/git/github.com/stesie/azure-cli-nix" ];

  # add equivalent to configuration.nix
  # network
  #   127.0.0.1 mkdocs
  # '';
  # services.httpd.virtualHosts.mkdocs = {
  #   documentRoot = "/home/kranium/work/gits/ops/site";
  # };
  # services.nfs.server.exports  = ''
  #   /home/kranium/possplay/puppet-controlrepo *(rw,insecure,no_root_squash,no_subtree_check,fsid=0)
  # '';

  environment.systemPackages = with pkgs;
    [
      azure-cli
      azure-storage-azcopy
      librarian-puppet-go
      mkdocs
      mysql
      networkmanager_l2tp
      networkmanager_openconnect
      openconnect_openssl
      softether
    ];

  services.mysql.enable = true;
  services.mysql.package = pkgs.mysql57;
  services.mysql.settings = {
    mysqld = {
      performance_schema = "on";
      innodb_strict_mode = false;
    };
  };

  services.softether.enable = true;
  services.softether.vpnclient.enable = true;

  users.extraUsers.kmendoza = {
     shell = "/run/current-system/sw/bin/bash";
     extraGroups = [ "wheel" "networkmanager" "audio" "docker" "vboxusers" "video" "lp" ];
     group = "users";
     createHome = true;
     home = "/home/kmendoza";
     isNormalUser = true;
   };

}
