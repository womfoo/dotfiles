{
  lib,
  pkgs,
  config,
  inputs,
  cell,
  ...
}:
{
  imports = [
    inputs.agenix.nixosModules.default
    cell.secrets.init-root-password.nixosModule
  ];

  users.users.root.hashedPasswordFile = cell.secrets.init-root-password.path config;

  environment.etc.hosts.mode = "0644";
  environment.systemPackages = with pkgs; [
    direnv
    gitFull
    iperf
    z-lua
  ];

  networking.firewall.allowedTCPPorts = [
    5201
  ];

  nix.settings.substituters = [
    "https://cache.nixos.org/"
    # "https://cache.iog.io"
    # "https://thefloweringash-armv7.cachix.org"
    # "https://nixcache.reflex-frp.org"
    # "https://static-haskell-nix.cachix.org"
    # "https://miso-haskell.cachix.org"
    # "https://nixiosk.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    # "cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    # "thefloweringash-armv7.cachix.org-1:v+5yzBD2odFKeXbmC+OPWVqx4WVoIVO6UXgnSAWFtso="
    # "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    # "static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU="
    # "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    # "nixiosk.cachix.org-1:A4kH9p+y9NjDWj0rhaOnv3OLIOPTbjRIsXRPEeTtiS4="
  ];
  nix.extraOptions = ''
    keep-outputs = true
    # extra-platforms = aarch64-linux
    experimental-features = nix-command flakes fetch-closure
    allow-import-from-derivation = true
    extra-sandbox-paths = /etc/skopeo/auth.json=/etc/nix/skopeo/auth.json
  '';
  # nix.settings.max-jobs = 1;
  # nix.settings.sandbox = false;
  nix.settings.trusted-users = [ "kranium" ];

  nix.settings.gc-keep-outputs = true;
  nix.settings.gc-keep-derivations = true;

  services.openssh.enable = true;
  # services.openssh.settings.PasswordAuthentication = false;
  # services.openssh.settings.PermitRootLogin = "prohibit-password";
  # services.openssh.settings.PasswordAuthentication = true;

  time.timeZone = "Australia/Sydney";

  system.configurationRevision = lib.mkIf (inputs.self ? rev) inputs.self.rev;
  system.nixos.label = lib.mkIf (inputs.self ? shortRev) (
    config.system.nixos.version + "-" + inputs.self.shortRev
  );
}
