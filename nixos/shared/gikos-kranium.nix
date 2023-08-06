{ ... }:
{

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

  programs.ssh.startAgent = true;
  programs.git.lfs.enable = true;

  security.sudo.wheelNeedsPassword = false;

  users.extraUsers.kranium = {
    name = "kranium";
    extraGroups = [ "wheel" "networkmanager" "audio" "docker" "vboxusers" "video" "lp" "dialout" "libvirtd" "kranium" "scanner" "cdrom" ];
    group = "users";
    uid = 2000;
    createHome = true;
    home = "/home/kranium";
    shell = "/run/current-system/sw/bin/bash";
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCv8Rdf8gqewljlONxIU/NoI+aQhA0UNQbAsif0gKqGLPG2QrZgPktgG3r0Fn6cKtuhy7iExfWmUafJU73Od/hj8DK6uxicHEXh5pv6DZc2DEwyC5orJHQOZLblo96u2xsBkVx/++Nq/2vW1aMN0Wg8/Vgal1fBcfJAT9XAFmiKXLZxIvxWWw0PZYil4QJtlVGwebXm1trPr7H9hV8l+Lse8Z/Xt38DzQJI7yV5m6ENxPL/xCFsMMgb27c+Xf6gJPq2DIcUOJiP7fOcHXWN2W4/+ApUH5adMhJ8Y8mT4CGcLqNhcHKSFzPaUQpfQ0vi3QJez1LYoHANu6Iy6q7HoIab kranium@silverspark"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICOWOMjEe3VCRGWeExqL6R0kjHZO5LyuKoysA2wY25Op kranium@blackslab"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDH+tjsc6Jsj3ZeJGke6bKR9gdoIXboMbVEu/USeCXY1DuMjcwGiOmmStPja99yy/T/XOmQ8Id12Cl3V5R4LeCmXtOmQVNHyNkxZUuiZ6+BIAxbE/cNQCp4NOKmBGPuMRaL7cuCA98zySAsSH8v5S0+7RELIBJ4g4gK6WnrvMrnKMxXaap5eodKUw3PBjhylaMDM6r3Ny/XK+/0cMBOlQGXjrn7JR9LsD7Xzd9aTVEeKiZSPGszi5o/sNRyrMF5UQZHLI3EhTduE6yxvEltPCUwEsQ8DfTmVWbOHtBSOJ8qRR5DwpH/r15o1/y/XpRLsFpy3bWeEJxBBuSFxt4WnIl1"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDYKG2PDhn+QeHVu596kNH57IziNANyhhXZIMcviWFzyd5lVLmyn41e3CUQkQUEguU+mgfrn1v3L/YAPTle/oA4bhwwoRj0nghZ13pkapNoXroAQUYlyXs4ngva1o5r3dZ7sqK+PboOFg1/HfrHKmVXqRRIOIKNUq0gLZLoe8Ga4UY/A5h+5NpHAo+uip4nPoKmHIX5e1ugAxXiKQ3sP+Ud3/wFftswyTDL1hllBeYuvbNzQIHvbtUr0e/gx54n1ETzcWu5BKrrBWyU4H4dwM9Cm0KJvhstdAcndj+BWeBZ+gmY4e6ESfZpLk9qO6LodZhXhFsavjET1miBCsFrUEHcSFBXpSzZ5djCFvN/QckYpXW46wouwbyZyBh2wgPc7o5jBdEMkCYEeec/TFfvuYAhLKASJM3vVK5vjAV1MAhnA9xKvR+jFLsi/6F88S4PxhYbIkBJ+YF3YAzdUVrduFUP8hFAZ6JTlzCAg8R+dNtCJHoaIkKsbaIgvUeiWBCTJG0= kranium@tessarion.local"
    ];
  };
  # FIXME: check if these still hold vs defaults
  users.extraGroups = { networkmanager = { } ; kranium = { gid = 2000; } ; kranium2 = { gid = 2001; }; telegraf = { }; } ;

  users.extraUsers.kranium2 = {
    name = "kranium2";
    extraGroups = [ "docker" ];
    group = "users";
    uid = 2001;
    createHome = true;
    isNormalUser = true;
  };

}
