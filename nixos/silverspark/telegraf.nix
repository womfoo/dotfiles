{ pkgs, ... }:
let
  secrets = import ./secrets.nix;
in
{

  systemd.services.telegraf.path = [ pkgs.procps ]; # telegraf procstat will fail without this
  services.telegraf.enable = true;
  services.telegraf.extraConfig = {
    agent = {
      interval = "30s";
      round_interval = true;
      metric_batch_size = 1000;
      metric_buffer_limit = 10000;
      collection_jitter = "0s";
      flush_interval = "30s";
      flush_jitter = "0s";
      #precision = "";
      debug = false;
      quiet = false;
      #logfile = "";
      #hostname = "";
      omit_hostname = false;
    };
    inputs = {
      statsd = {
        delete_timings = true;
        service_address = ":8125";
      };
      cpu = {
        percpu = true;
        totalcpu = true;
        collect_cpu_time = false;
      };
      disk = { };
      diskio = { };
      kernel = { };
      mem = { };
      net = { };
      netstat = { };
      nstat = {
        proc_net_netstat  = "/proc/net/netstat";
        proc_net_snmp     = "/proc/net/snmp";
        proc_net_snmp6    = "/proc/net/snmp6";
        dump_zeros        = true;
      };
      system = { };
      swap = { };
      processes = { };
      procstat = {
        exe = ".*"; # limit this in the future, this sends everything
      };
    };
    outputs = { influxdb = { database = "metrics";
                             urls = [ "https://influxdb.gikos.net:8086" ];
                             username = secrets.remote_influxd.username;
                             password = secrets.remote_influxd.pass;
              };
    };
  };

}
