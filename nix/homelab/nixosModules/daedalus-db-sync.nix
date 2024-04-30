# hacky way to run db-sync with daedalus' cardano-node
# which saves abit of disk space
{
  lib,
  pkgs,
  config,
  ...
}:
let
  # fix me try to run concurrently
  db-sync-network = "mainnet";
  # db-sync-network = "preprod";
  user = "kranium";
in
{
  services.cardano-db-sync = {
    enable = true;
    cluster = db-sync-network;
    socketPath = "/home/${user}/.local/share/Daedalus/${db-sync-network}/cardano-node.socket";
    stateDir = "/var/lib/cexplorer-${db-sync-network}";
    postgres = {
      user = "cdbsync-${db-sync-network}";
      database = "cdbsync-${db-sync-network}";
    } ;
  };
  users.groups.cardano-node = {
    gid = 5000;
  };
  users.extraUsers."${user}" = {
    homeMode = "750";
    group = lib.mkDefault "cardano-node";
  };
  services.postgresql ={
    enable = true;
    enableTCPIP = false;
    ensureDatabases = [
      # "${config.services.cardano-db-sync.postgres.database}"
      "cdbsync-preprod"
      "cdbsync-mainnet"
    ];
    ensureUsers = [
      # {
      #   name = "${config.services.cardano-db-sync.postgres.user}";
      #   ensureDBOwnership = true;
      # }
      {
        name = "cdbsync-preprod";
        ensureDBOwnership = true;
      }
      {
        name = "cdbsync-mainnet";
        ensureDBOwnership = true;
      }
    ];
    identMap = ''
      users cardano-db-sync cdbsync-mainnet
      users cardano-db-sync cdbsync-preprod
      users postgres postgres
    '';
    # users cardano-db-sync ${config.services.cardano-db-sync.postgres.user}
    authentication = ''
      local all all ident map=users
    '';
  };
}
