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
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCv8Rdf8gqewljlONxIU/NoI+aQhA0UNQbAsif0gKqGLPG2QrZgPktgG3r0Fn6cKtuhy7iExfWmUafJU73Od/hj8DK6uxicHEXh5pv6DZc2DEwyC5orJHQOZLblo96u2xsBkVx/++Nq/2vW1aMN0Wg8/Vgal1fBcfJAT9XAFmiKXLZxIvxWWw0PZYil4QJtlVGwebXm1trPr7H9hV8l+Lse8Z/Xt38DzQJI7yV5m6ENxPL/xCFsMMgb27c+Xf6gJPq2DIcUOJiP7fOcHXWN2W4/+ApUH5adMhJ8Y8mT4CGcLqNhcHKSFzPaUQpfQ0vi3QJez1LYoHANu6Iy6q7HoIab kranium@silverspark"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDYKG2PDhn+QeHVu596kNH57IziNANyhhXZIMcviWFzyd5lVLmyn41e3CUQkQUEguU+mgfrn1v3L/YAPTle/oA4bhwwoRj0nghZ13pkapNoXroAQUYlyXs4ngva1o5r3dZ7sqK+PboOFg1/HfrHKmVXqRRIOIKNUq0gLZLoe8Ga4UY/A5h+5NpHAo+uip4nPoKmHIX5e1ugAxXiKQ3sP+Ud3/wFftswyTDL1hllBeYuvbNzQIHvbtUr0e/gx54n1ETzcWu5BKrrBWyU4H4dwM9Cm0KJvhstdAcndj+BWeBZ+gmY4e6ESfZpLk9qO6LodZhXhFsavjET1miBCsFrUEHcSFBXpSzZ5djCFvN/QckYpXW46wouwbyZyBh2wgPc7o5jBdEMkCYEeec/TFfvuYAhLKASJM3vVK5vjAV1MAhnA9xKvR+jFLsi/6F88S4PxhYbIkBJ+YF3YAzdUVrduFUP8hFAZ6JTlzCAg8R+dNtCJHoaIkKsbaIgvUeiWBCTJG0= kranium@tessarion.local"
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
