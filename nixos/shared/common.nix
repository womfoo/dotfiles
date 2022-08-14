{ lib, pkgs, config, ... }: {

  environment.etc.hosts.mode = "0644";

  nix.settings.substituters = [
    "https://hydra.iohk.io"
    "https://cache.iog.io"
    "https://cache.nixos.org/"
    # "https://thefloweringash-armv7.cachix.org"
    # "https://nixcache.reflex-frp.org"
    # "https://static-haskell-nix.cachix.org"
    # "https://miso-haskell.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    # "thefloweringash-armv7.cachix.org-1:v+5yzBD2odFKeXbmC+OPWVqx4WVoIVO6UXgnSAWFtso="
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    # "static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    # "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    "cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
  nix.extraOptions = ''
    keep-outputs = true
    extra-platforms = aarch64-linux
    experimental-features = nix-command flakes
    allow-import-from-derivation = true
    extra-sandbox-paths = /etc/skopeo/auth.json=/etc/nix/skopeo/auth.json
  '';

  programs.gnupg.agent.enable = true;
  programs.ssh.startAgent = true;

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.permitRootLogin = "prohibit-password";

  time.timeZone = "Australia/Sydney";

}
