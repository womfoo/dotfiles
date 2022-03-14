{ lib, pkgs, config, ... }: {

  environment.etc.hosts.mode = "0644";

  programs.gnupg.agent.enable = true;
  programs.ssh.startAgent = true;

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.permitRootLogin = "prohibit-password";

  time.timeZone = "Australia/Sydney";

}
