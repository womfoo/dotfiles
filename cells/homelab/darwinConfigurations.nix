{

  tessarion = {pkgs, ...}: {
    bee.system = "aarch64-darwin";

    bee.darwin = inputs.darwin;
    bee.pkgs = import inputs.nixos {
      system = "aarch64-darwin";
    };

    services.nix-daemon.enable = true;

    # nix.linux-builder.enable = true;
    # nix.linux-builder.package = pkgs.linux-builder;
    nix.settings.trusted-users = [ "kranium" ];

    nix.settings.substituters = [
      "https://cache.iog.io"
      "https://cache.nixos.org/"
    ];
    nix.settings.trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

    #  users.users.kranium.packages = [
    #    pkgs.jq
    #    pkgs.ncdu
    #  ];

    environment.systemPackages = with pkgs;
      [
        # chromium
        # firefox-bin
        # rsync
        # slack
        direnv
        emacs
        ghc
        gitFull
        hledger
        hledger-web
        htop
        iterm2
        jq
        ncdu
        rnix-lsp
        z-lua
      ];

  };

}
