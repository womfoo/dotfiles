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
  # db-sync-network = "mainnet";
  db-sync-network = "preprod";
  user = "kranium";
  socketPath = "/home/${user}/.local/share/Daedalus/${db-sync-network}/cardano-node.socket";
  fixsocketperms = envName: pkgs.writeScriptBin "fix-${envName}-socket-perms" ''
    sudo chmod 770 ${socketPath}
    sudo chown ${user}:cardano-node ${socketPath}
  '';
  wallet_wrapper = envName: pkgs.writeScriptBin "cardano-wallet-${envName}" ''
    CARDANO_WALLET_EKG_PORT=8070
    CARDANO_WALLET_PROMETHEUS_PORT=8080
    WALLET_STATE_DIR=/home/''$USER/cardano-wallet-state
    mkdir -p ''$WALLET_STATE_DIR
    cardano-wallet serve \
      --node-socket /home/${user}/.local/share/Daedalus/${envName}/cardano-node.socket \
      --database /home/kranium/cardano-wallet-state/${envName} \
      ${if envName == "mainnet" then "--mainnet" else "--testnet"}
  '';
in
{
  environment.systemPackages = with pkgs; [
    (fixsocketperms "mainnet")
    (fixsocketperms "preprod")
    (wallet_wrapper "mainnet")
    (wallet_wrapper "preprod")
    # extrabits
  ];

  services.cardano-db-sync = {
    enable = true;
    cluster = db-sync-network;
    stateDir = "/var/lib/cexplorer-${db-sync-network}";
    inherit socketPath;
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
    group = lib.mkForce "cardano-node";
  };
  services.postgresql ={
    enable = true;
    enableTCPIP = true;
    ensureDatabases = [
      # "${config.services.cardano-db-sync.postgres.database}"
      "cdbsync-preprod"
      "cdbsync-mainnet"
      "identus-preprod"
      "identus-mainnet"
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
      {
        name = "identus-preprod";
        ensureDBOwnership = true;
      }
      {
        name = "identus-mainnet";
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
