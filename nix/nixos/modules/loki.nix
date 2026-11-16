{
  lib,
  pkgs,
  config,
  ...
}:
{
  services.loki = {
    enable = true;
    configuration = {
      auth_enabled = false;
      server = {
        http_listen_port = 53100;
        grpc_listen_port = 9096;
        grpc_server_max_concurrent_streams = 1000;
        log_level = "debug";
      };
      common = {
        path_prefix = config.services.loki.dataDir;
        storage.filesystem = {
          chunks_directory = "${config.services.loki.dataDir}/chunks";
          rules_directory = "${config.services.loki.dataDir}/rules";
        };
        replication_factor = 1;
        ring.kvstore.store = "inmemory";
      };
      pattern_ingester = {
        enabled = true;
        metric_aggregation.loki_address = "localhost:3100";
      };
      schema_config.configs = [
        {
          from = "2020-11-08";
          store = "tsdb";
          object_store = "filesystem";
          schema = "v13";
          index.prefix = "index_";
          index.period = "24h";
        }
      ];
      frontend.encoding = "protobuf";
      limits_config = {
        reject_old_samples = true;
        reject_old_samples_max_age = "8760h";
        retention_period = "20000h";
      };
    };
  };
}
