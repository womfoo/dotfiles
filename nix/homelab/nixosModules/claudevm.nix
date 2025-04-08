{
  config,
  lib,
  pkgs,
  ...
}:
{
  microvm = {
    hypervisor = "qemu";
    graphics.enable = true;
    interfaces = [
      {
        type = "tap";
        # type = "user";
        id = "tun01"; # works with user
        mac = "00:00:00:00:00:02";
      }
    ];

    shares = [
      {
        # use proto = "virtiofs" for MicroVMs that are started by systemd
        proto = "9p";
        tag = "ro-store";
        # a host's /nix/store will be picked up so that no
        # squashfs/erofs will be built for it.
        source = "/nix/store";
        mountPoint = "/nix/.ro-store";
      }
    ];

  };

  systemd.network.enable = true;
  networking.hostName = "graphical-microvm";
  system.stateVersion = lib.trivial.release;

  services.getty.autologinUser = "user";
  users.users.user = {
    password = "";
    group = "user";
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "video"
    ];
  };
  users.groups.user = { };
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # environment.sessionVariables = {
  #   WAYLAND_DISPLAY = "wayland-1";
  #   DISPLAY = ":0";
  #   QT_QPA_PLATFORM = "wayland"; # Qt Applications
  #   GDK_BACKEND = "wayland"; # GTK Applications
  #   XDG_SESSION_TYPE = "wayland"; # Electron Applications
  #   SDL_VIDEODRIVER = "wayland";
  #   CLUTTER_BACKEND = "wayland";
  # };

  # systemd.user.services.wayland-proxy = {
  #   enable = true;
  #   description = "Wayland Proxy";
  #   serviceConfig = with pkgs; {
  #     # Environment = "WAYLAND_DISPLAY=wayland-1";
  #     ExecStart = "${wayland-proxy-virtwl}/bin/wayland-proxy-virtwl --virtio-gpu --x-display=0 --xwayland-binary=${xwayland}/bin/Xwayland";
  #     Restart = "on-failure";
  #     RestartSec = 5;
  #   };
  #   wantedBy = [ "default.target" ];
  # };

  imports = [
    cell.nixosModules.common
  ];

  environment.systemPackages = with pkgs; [
    firefox-bin
    inputs.claude.packages.claude-desktop-with-fhs
    xdg-utils # Required
  ];
  hardware.graphics.enable = true;

  services.spice-vdagentd.enable = true;

  microvm.qemu.extraArgs = [
    "-vnc"
    ":0"
    "-vga"
    "qxl"
    # needed for mounse/keyboard input via vnc
    "-device"
    "virtio-keyboard"
    "-usb"
    "-device"
    "usb-tablet,bus=usb-bus.0"
  ];

  #       microvm.qemu.extraArgs = [

  #       # "-display" "gtk,gl=off"
  #       # "-device=:0" "virtio-vga-gl"
  #       # "-device" "qemu-xhci"
  #       # "-device" "usb-tablet"
  #       # "-device" "usb-kbd"

  #         # "-vnc" ":0"

  #       #   "-vnc" ":0"
  #       #   "-vga" "qxl"

  #       #   # "-display spice"
  #       #   # needed for mounse/keyboard input via vnc
  #       #   "-device" "virtio-keyboard"
  #       #   "-usb"
  #       #   "-device" "usb-tablet,bus=usb-bus.0"
  # # "-vga" "qxl"
  # # "-spice" "port=5924,disable-ticketing=on"
  # # "-device" "virtio-serial" "-chardev" "spicevmc,id=vdagent,debug=0,name=vdagent"
  # # "-device" "virtserialport,chardev=vdagent,name=com.redhat.spice.0"

  #       ];

  services.xserver = {
    enable = true;
    desktopManager.xfce.enable = true;
    displayManager.autoLogin.user = "user";
  };

}
