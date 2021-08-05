{
  network.description = "australia nodes";

  au01 =
    { config, pkgs, ... }:
    { deployment.targetHost = "149.28.180.243";
      deployment.targetUser = "kranium";
    };
}
