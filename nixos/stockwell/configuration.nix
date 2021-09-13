# FIXME: this is still pretty rough, based on silverspark
{ config, options, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../shared/hydra.nix
      ../shared/gikos-kranium.nix
      ../shared/desktop-apps.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.timeout = 4;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    # nameservers = [ "8.8.8.8" "8.8.4.4" ];
    hostName = "stockwell";
    networkmanager = {
      enable = true;
      dns = "none";
    };
    firewall.logRefusedPackets = true;
    firewall.allowedTCPPorts = [
      config.services.hydra.port # FIXME: move behind apache
    ];
  };

  services.acpid.enable = true;
  services.upower.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.xserver.enable = true;
  services.xserver.autorun = false;
  services.xserver.displayManager.startx.enable = true;
  
  # FIXME: see if we can replace the ff as I would like to try just using startx
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.displayManager.defaultSession = "none+xmonad";

  hardware.cpu.intel.updateMicrocode = true;

  #this tends to overheat
  powerManagement.cpuFreqGovernor = "performance";

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull; # we need to use the full package for bluetooth support
  #for wireless headset
  # hardware.pulseaudio.extraConfig = ''
  #   load-module module-switch-on-connect
  # '';

  hardware.bluetooth.enable = true;

  hardware.opengl.extraPackages = with pkgs; [ libvdpau-va-gl vaapiVdpau ];

  time.timeZone = "Australia/Sydney";

  #services.flatpak.enable = true;
  xdg.portal.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.libvirtd.enable = true;

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
    packageOverrides = pkgs: {
      # 11-Sep-2021 works 6d8be3549ca453a3a60d73ddcf368c84742dadb0
      nur = import /home/kranium/git/github.com/nix-community/nur-combined {
        inherit pkgs;
      };
    };
  };

  environment.etc.hosts.mode = "0644";

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

  #services.influxdb.enable = true;

  #iphone mounting needs
  services.usbmuxd.enable = true;

  system.stateVersion = "21.1";
  nix.package = pkgs.nixUnstable;

  nix.extraOptions = ''
    keep-outputs = true
    extra-platforms = aarch64-linux
    experimental-features = nix-command flakes
  '';

  services.arbtt.enable = true;

  programs.gnupg.agent.enable = true;

  programs.tmux = {
    enable = true;
    historyLimit = 50000;
    extraConfig = ''
      run-shell ${pkgs.tmuxPlugins.logging}/share/tmux-plugins/logging/logging.tmux
    '';
  };

}
