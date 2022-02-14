{ config, pkgs, ... }:
{

 services.postgresql = {
   enable = true;
   extraPlugins = [ pkgs.timescaledb ];
   settings = {
     shared_preload_libraries = "timescaledb";
   };
 };

}
