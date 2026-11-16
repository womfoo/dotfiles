{ config, pkgs, ... }:

let
  # Path literal: gets copied into the Nix store at build time.
  # Or use a string like "/home/you/Pictures/fake.jpg" to read it at runtime.
  photo = ./kaye_scrot.png;
  width = 3840;
  height = 2160;
  fps = 15;
in
{
  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.kernelModules = [ "v4l2loopback" ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=20 card_label="Desktop Camera" exclusive_caps=1
  '';

  systemd.services.fake-camera = {
    description = "Fake V4L2 camera from a still image";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-modules-load.service" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.ffmpeg}/bin/ffmpeg -hide_banner -loglevel warning \
          -re -loop 1 -framerate ${toString fps} -i ${photo} \
          -vf "scale=${toString width}:${toString height}:force_original_aspect_ratio=decrease,pad=${toString width}:${toString height}:(ow-iw)/2:(oh-ih)/2,format=yuv420p" \
          -f v4l2 /dev/video20
      '';
      Restart = "always";
      RestartSec = 2;
    };
  };

  # Optional: handy for testing with `v4l2-ctl --list-devices`
  environment.systemPackages = [ pkgs.v4l-utils ];
}
