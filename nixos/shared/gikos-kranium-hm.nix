{ pkgs, nixpkgs-stable, ... }:
{
  home-manager.useGlobalPkgs = true;
  home-manager.users.kranium = {
     home.packages = [ pkgs.lolcat ];
     # home.stateVersion = "22.05";
     home.stateVersion = "22.11";
     programs = {
       firefox = {
         enable = true;
         extensions = with pkgs.nur.repos.rycee.firefox-addons; [
           foxyproxy-standard
           ublock-origin
           vimium
         ];
         # extensions will not work if you dont have profiles defined
         profiles.kranium = {
           id = 0;
           isDefault = true;
           path = "s0h80mj1.default-1471996773737"; # FIXME: generalize outside silverspark
         };
         profiles.work = {
           id = 1;
         };
         # package = nixpkgs-stable.firefox;
       };
       git = {
         enable = true;
         # userEmail = "kranium@gikos.net";
         userEmail = "kraniumgikos.mendoza@iohk.io";
         userName  = "Kranium Gikos Mendoza";
         signing = {
           signByDefault = true;
           key ="30079627378B190345DAEF17A578D4096D011982";
         };
         extraConfig = {
           push.autoSetupRemote =  true;
         };
       };
       man.enable = false;
     };
     manual.manpages.enable = false;
  };
}
