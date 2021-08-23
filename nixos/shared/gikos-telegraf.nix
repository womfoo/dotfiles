{ lib, pkgs, config, ... }: {

  services.telegraf.enable = true;
  services.telegraf.extraConfig = {
    # global_tags = {
    #   user = "root";
    # };
    /*
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
      conntrack = {};
      cpu = {
        percpu = true;
        totalcpu = true;
        collect_cpu_time = false;
      };
      disk = { };
      diskio = { };
      interrupts = {};
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
        pid_finder = "pgrep";
      };
    };
    outputs = { influxdb = { database = "metrics";
                             urls = [ "http://influxdb.syd0.gikos.net:8086" ];
              };
    };
    */
  };

}
