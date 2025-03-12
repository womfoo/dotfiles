{ ... }:
{
  security.sudo.wheelNeedsPassword = false;

  users.extraUsers.kranium = {
    name = "kranium";
    extraGroups = [
      "audio"
      "cdrom"
      "dialout"
      "docker"
      "i2c"
      "kranium"
      "kvm"
      "libvirtd"
      "lp"
      "networkmanager"
      "paperless"
      "qemu-libvirtd"
      "scanner"
      "users"
      "vboxusers"
      "video"
      "wheel"
    ];
    group = "users";
    uid = 2000;
    createHome = true;
    home = "/home/kranium";
    shell = "/run/current-system/sw/bin/bash";
    isNormalUser = true;
    openssh.authorizedKeys.keys = with inputs.lihim.pubkeys.constants.admins; [
      kranium
      kraniumarm
      kraniummac
    ];
  };
  security.polkit.enable = true;

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (( action.id == "org.freedesktop.systemd1.manage-units"
         && subject.user == "kranium")) {
         return polkit.Result.YES;
      }
    });
  '';
}
