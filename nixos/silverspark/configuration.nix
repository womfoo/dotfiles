{ config, options, lib, pkgs, ... }:

let
  grafana-mqtt-datasource = pkgs.callPackage /home/kranium/git/github.com/grafana/mqtt-datasource {};
  noplay = false;
  # noplay = true;
in
{
  nix.package = pkgs.nixUnstable;
  nix.settings.cores = 4;
  nix.settings.max-jobs = lib.mkDefault 8;
  nix.settings.substituters = lib.mkForce [
    "https://cache.nixos.org/"
    # "https://thefloweringash-armv7.cachix.org"
    # "https://nixcache.reflex-frp.org"
    # "https://static-haskell-nix.cachix.org"
    # "https://hydra.iohk.io"
    # "https://miso-haskell.cachix.org"
  ];
  nix.settings.trusted-public-keys = lib.mkForce [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    # "thefloweringash-armv7.cachix.org-1:v+5yzBD2odFKeXbmC+OPWVqx4WVoIVO6UXgnSAWFtso="
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    # "static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU="
    # "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    # "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
  ];
  nix.extraOptions = ''
    keep-outputs = true
    extra-platforms = aarch64-linux
    experimental-features = nix-command flakes
  '';
  nix.distributedBuilds = true;

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./telegraf.nix
      ../shared/hydra.nix
      ../shared/paperless.nix
      ../shared/gikos-kranium.nix
      ../shared/gikos-kranium-hm.nix
      ../shared/desktop-apps.nix
      # ./old-work.nix
      #./asterisk-test.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_latest; # 2021-11-16: 5.15.1 not working with nvidia
  # boot.kernelPackages = pkgs.linuxPackages_5_14;

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    # nameservers = [ "8.8.8.8" "8.8.4.4" ];
    hostName = "silverspark";
    networkmanager = {
      enable = true;
      dns = "none";
    };
    firewall.logRefusedPackets = true;
    firewall.allowedTCPPorts = [
      111 # nfs
      2049 # nfs
      config.services.nfs.server.statdPort
      config.services.nfs.server.mountdPort
      config.services.nfs.server.lockdPort
      config.services.nix-serve.port # FIXME: move behind apache
      config.services.hydra.port # FIXME: move behind apache
    ];
    firewall.allowedUDPPorts = [
      53 # dnsmasq
      67 # dhcpd
      111 # nfs
      2049 # nfs
      config.services.nfs.server.statdPort
      config.services.nfs.server.mountdPort
      config.services.nfs.server.lockdPort
    ];
    firewall.allowedUDPPortRanges = [
      # https://blog.g3rt.nl/allow-google-chromecast-host-firewall-iptables.html
      { from = 32768; to = 61000; }
    ];

    extraHosts = ''
      127.0.0.1 silverspark.gikos.net
      127.0.0.1 geolite.maxmind.com
    '' +lib.optionalString noplay ''
      127.0.0.1 laarc.io
      127.0.0.1 lobste.rs
      127.0.0.1 news.ycombinator.com
      127.0.0.1 slashdot.org
      127.0.0.1 twitter.com
      127.0.0.1 www.youtube.com
    '';
  };

  services.acpid.enable = true;
  services.upower.enable = true;

  # services.mbpfan.enable = true;
  # services.mbpfan.minFanSpeed = 4000; # noisy
  # services.mbpfan.lowTemp = 55;   # try ranges 55-63, default is 63
  # services.mbpfan.highTemp = 58;  # try ranges 58-66, default is 66
  # services.mbpfan.maxTemp = 78;   # do not set it > 90, default is 86
  # services.mbpfan.pollingInterval = 1;
  # services.mbpfan.verbose = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplipWithPlugin ];

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplipWithPlugin ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.libinput.enable = true;
  services.xserver.synaptics.enable = false;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.sddm.enable = true;

  services.xserver.screenSection = ''
    Option "metamodes" "nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
    Option         "AllowIndirectGLXProtocol" "off"
    Option         "TripleBuffer" "on"
  '';

  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  services.xserver.videoDrivers = [ "nvidia" ];
  #services.xserver.videoDrivers = [ "nvidiaLegacy470" ];
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.displayManager.defaultSession = "none+xmonad";

  users.extraUsers.telegraf = {
    extraGroups = [ "telegraf" ];
  };
  hardware.cpu.intel.updateMicrocode = true;

  #this tends to overheat
  powerManagement.cpuFreqGovernor = "performance"; # defaults to powersave

  hardware.facetimehd.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    # config.pipewire = {
    #   "context.properties" = {
    #     "default.clock.rate" = 192000;
    #     "default.clock.allowed-rates" = [ 44100 48000 96000 192000 ];
    #   };
    # };
  };
  # hardware.pulseaudio.enable = true;
  # hardware.pulseaudio.package = pkgs.pulseaudioFull; # we need to use the full package for bluetooth support
  #for wireless headset
  # hardware.pulseaudio.extraConfig = ''
  #   load-module module-switch-on-connect
  # '';

  hardware.bluetooth.enable = true;

  # for steam to work
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };
  hardware.opengl.extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ];

  time.timeZone = "Australia/Sydney";

  virtualisation.docker.enable = true;

  #FIXME: disable flatpak permanently? for 22.05
  # Setting xdg.portal.enable to true requires a portal implementation in xdg.portal.extraPortals such as xdg-desktop-portal-gtk or xdg-desktop-portal-kde
  # To use Flatpak you must enable XDG Desktop Portals with xdg.portal.enable.
  #services.flatpak.enable = true;
  #xdg.portal.enable = true;
  # virtualisation.virtualbox.host.enable = true;
  virtualisation.libvirtd.enable = true;

  nixpkgs = {
    overlays = [
      (import ../shared/overlay-x86_64.nix)
    ];
    config = {
      allowBroken = true;
    allowUnfree = true;
    packageOverrides = pkgs: {
      # 11-Sep-2021 works 6d8be3549ca453a3a60d73ddcf368c84742dadb0
      nur = import /home/kranium/git/github.com/nix-community/nur-combined {
        inherit pkgs;
      };
    #   haskellPackages = pkgs.haskellPackages.override {
    #         overrides = hsSelf: hsSuper: {
    #           postgres-websockets = hsSelf.haskell.lib.doJailbreak hsSuper.postgres-websockets;
    #           sproxy2 = hsSelf.callPackage /home/kranium/git/github.com/ip1981/sproxy2/default.nix { } ;
    #           docopt = hsSelf.callPackage /home/kranium/git/github.com/jefdaj/docopt.hs/default.nix { };
    #         };
    #   };
    };
    # permittedInsecurePackages = [
    #   "openssl-1.0.2u"
    # ];
    };
  };

  programs.light.enable = true;
  programs.kbdlight.enable = true;

  #services.dockerRegistry.enable = true;
  environment.etc.hosts.mode = "0644";

  services.nfs.server = {
    enable     = true;
    createMountPoints = true;
    exports    = ''
      /home/kranium/Downloads *(ro,no_root_squash,no_subtree_check,fsid=1)
    '';
    statdPort  = 4000;
    lockdPort  = 4001;
    mountdPort = 4002;
  };

  # https://github.com/NixOS/nixpkgs/issues/14390 2/2
  environment.pathsToLink = [ "/share" ];

  environment.interactiveShellInit = ''
    # TERM=rxvt-unicode-256color seen in remote which makes backspace broken
    # use remove the 'unicode' part for now
    TERM=rxvt-256color
    # append history instead of overwrite
    shopt -s histappend
    # big history, record everything
    export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
    export HISTSIZE=-1
    export HISTFILESIZE=-1
 '';

  services.xserver.displayManager.sessionCommands = ''
     # This allows GTK to load SVG icons.
    export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)
  '';

  programs.ssh.startAgent = true;

  services.influxdb.enable = true;

  #iphone mounting needs
  services.usbmuxd.enable = true;

  boot.supportedFilesystems = [ "btrfs" "jfs" "reiserfs" "xfs" ];
  system.stateVersion = "21.11";

  services.postgresql.enable = true;
  services.postgresql.enableTCPIP = true;
  services.postgresql.authentication = ''
    host all all 172.20.10.0/24 trust
    host all all 172.28.0.0/24 trust
    host all all 172.16.0.0/16 trust
    host all all 172.17.0.0/16 trust
  '';


  services.arbtt.enable = true;
  # services.arbtt.package = pkgs.haskell.packages.ghc8104.arbtt;

  environment.variables = {
      PATH = "$PATH:/home/kranium/bin";
  };

  programs.gnupg.agent.enable = true;

  programs.tmux = {
    enable = true;
    historyLimit = 50000;
    extraConfig = ''
      run-shell ${pkgs.tmuxPlugins.logging}/share/tmux-plugins/logging/logging.tmux
    '';
  };

  services.grafana = {
    enable = true;
    # declarativePlugins = [  ];
    # declarativePlugins = with pkgs.grafanaPlugins; [ grafana-mqtt-datasource grafadruid-druid-datasource ];
  };

}
