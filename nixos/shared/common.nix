{ lib, pkgs, config, ... }: {

  environment.etc.hosts.mode = "0644";

  programs.gnupg.agent.enable = true;
  programs.ssh.startAgent = true;

  services.openssh.enable = true;

  time.timeZone = "Australia/Sydney";

}
