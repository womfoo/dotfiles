{ pkgs, ... }:
{

  imports = [
    (import /home/kranium/git/github.com/nix-community/home-manager/nixos)
  ];

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
