sudo nixos-firewall-tool open udp 67
sudo nixos-firewall-tool open udp 69
sudo nixos-firewall-tool open udp 4011
sudo nixos-firewall-tool open tcp 67
sudo nixos-firewall-tool open tcp 69
sudo nixos-firewall-tool open tcp 4011
sudo nixos-firewall-tool open tcp 64172
sudo pixiecore \
      boot netboot-dreadfort/bzImage \
      netboot-dreadfort/initrd \
      --cmdline "init=/nix/store/pkkc00hxwdb3syaa74sk05c9sggh8p6i-nixos-system-dreadfort-24.05pre-git/init loglevel=4" \
      --debug --dhcp-no-bind \
      --port 64172 --status-port 64172



     #--cmdline "$(cat netboot-dreadfort/init)" \
