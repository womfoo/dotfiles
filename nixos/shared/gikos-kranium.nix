{ lib, pkgs, config, ... }: {

  imports = [
    (import /home/kranium/git/github.com/nix-community/home-manager/nixos)
  ];

  security.sudo.wheelNeedsPassword = false;

  users.extraUsers.kranium = {
    name = "kranium";
    extraGroups = [ "wheel" "networkmanager" "audio" "docker" "vboxusers" "video" "lp" "dialout" "libvirtd" "kranium" ];
    group = "users";
    uid = 2000;
    createHome = true;
    home = "/home/kranium";
    shell = "/run/current-system/sw/bin/bash";
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCv8Rdf8gqewljlONxIU/NoI+aQhA0UNQbAsif0gKqGLPG2QrZgPktgG3r0Fn6cKtuhy7iExfWmUafJU73Od/hj8DK6uxicHEXh5pv6DZc2DEwyC5orJHQOZLblo96u2xsBkVx/++Nq/2vW1aMN0Wg8/Vgal1fBcfJAT9XAFmiKXLZxIvxWWw0PZYil4QJtlVGwebXm1trPr7H9hV8l+Lse8Z/Xt38DzQJI7yV5m6ENxPL/xCFsMMgb27c+Xf6gJPq2DIcUOJiP7fOcHXWN2W4/+ApUH5adMhJ8Y8mT4CGcLqNhcHKSFzPaUQpfQ0vi3QJez1LYoHANu6Iy6q7HoIab kranium@silverspark"
    ];
  };
  # FIXME: check if these still hold vs defaults
  users.extraGroups = { networkmanager = { } ; kranium = { gid = 2000; } ; telegraf = { }; } ;

  home-manager.useGlobalPkgs = true;
  home-manager.users.kranium = {
     home.packages = [ pkgs.lolcat ];
     programs = {
       firefox = {
         enable = true;
         extensions = with pkgs.nur.repos.rycee.firefox-addons; [
           ublock-origin
           vimium
         ];
         # extensions will not work if you dont have profiles defined
         profiles.kranium = {
           id = 0;
           isDefault = true;
           path = "s0h80mj1.default-1471996773737"; # FIXME: generalize outside silverspark
         };
       };
       git = {
         enable = true;
         userEmail = "kranium@gikos.net";
         userName  = "Kranium Gikos Mendoza";
       };
     };
  };
}
