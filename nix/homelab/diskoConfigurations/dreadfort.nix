{
  disko.devices = {
    disk = {
      z = {
        type = "disk";
        device = "/dev/disk/by-id/wwn-0x500a0751e89731d2";
        content = {
          type = "table";
          format = "gpt";
          partitions = [
            {
              name = "ESP";
              start = "0";
              end = "1000MiB";
              fs-type = "fat32";
              # fs-type = "EF00";
              bootable = true;
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            }
            {
              name = "windows";
              start = "1000MiB";
              end = "100GiB";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/windows";
              };
            }
            {
              name = "linux";
              start = "100GiB";
              end = "200GiB";
              content = {
                type = "filesystem";
                format = "ntfs";
                mountpoint = "/";
              };
            }
            {
              name = "zfs";
              start = "200GiB";
              end = "100%";
              content = {
                type = "zfs";
                pool = "tank";
              };
            }
          ];
        };
      };
    };
    zpool = {
      tank = {
        type = "zpool";
        mountpoint = "/tank";
        rootFsOptions = {
          canmount = "off";
        };
        datasets = {
          scratch = {
            type = "zfs_fs";
            mountpoint = "/tank/scratch";
          };
          rancher = {
            type = "zfs_fs";
            mountpoint = "/var/lib/rancher";
          };
        };
      };
    };
  };
}
