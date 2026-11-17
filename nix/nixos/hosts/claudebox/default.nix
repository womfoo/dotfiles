# Lightweight sandbox VM for coding agents (Claude Code).
#
#   nix run .#claudebox                 # boot it (from the repo root; state lands in ./claudebox-*.img)
#   ssh -p 2232 claude@localhost        # get in
#
# Only the repos listed in `repos` are visible, mounted read-write at the same
# path as on the host so git remotes, flake paths and error messages line up.
# Nothing else from the host (ssh keys, ~/.config, secrets, lihim) is shared.
{
  pkgs,
  inputs,
  ...
}:
let
  # host paths shared into the VM (read-write, same path inside)
  repos = [
    "/home/kranium/git/github.com/womfoo/dotfiles"
    "/home/kranium/git/github.com/womfoo/nixpkgs"
    "/home/kranium/git/github.com/womfoo/fake"
  ];

  # 9p shares are accessed as the host user running qemu, so the guest user
  # gets the same uid to keep ownership/permissions sane on both sides.
  hostUid = 2000;

  tagOf = path: "repo-" + builtins.substring 0 20 (builtins.hashString "sha256" path);
in
{
  imports = [ inputs.microvm.nixosModules.microvm ];

  networking.hostName = "claudebox";
  system.stateVersion = "26.05";
  nixpkgs.config.allowUnfree = true; # claude-code

  microvm = {
    hypervisor = "qemu";
    vcpu = 4;
    mem = 8192;

    # user-mode networking: no root, no tap/bridge on the host
    interfaces = [
      {
        type = "user";
        id = "claudebox";
        mac = "02:00:00:c1:a0:de";
      }
    ];
    forwardPorts = [
      {
        from = "host";
        host.address = "127.0.0.1";
        host.port = 2232;
        guest.port = 22;
      }
    ];

    shares = [
      {
        proto = "9p";
        tag = "ro-store";
        source = "/nix/store";
        mountPoint = "/nix/.ro-store";
      }
    ]
    ++ map (path: {
      proto = "9p";
      tag = tagOf path;
      source = path;
      mountPoint = path;
    }) repos;

    # persistent state, created next to wherever you `nix run` it
    volumes = [
      {
        image = "claudebox-home.img";
        mountPoint = "/home/claude";
        size = 20480;
      }
      {
        image = "claudebox-nix.img";
        mountPoint = "/nix/.rw-store";
        size = 40960;
      }
    ];
    writableStoreOverlay = "/nix/.rw-store";
  };

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    trusted-users = [ "claude" ];
  };

  users.users.claude = {
    isNormalUser = true;
    uid = hostUid;
    extraGroups = [ "wheel" ];
    # add your key(s) here; root/password login stays disabled
    openssh.authorizedKeys.keys = [ ];
  };
  security.sudo.wheelNeedsPassword = false;

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.PermitRootLogin = "no";
  };

  # repos are owned by the host uid; don't trip git's safe.directory check
  environment.etc.gitconfig.text = ''
    [safe]
      directory = *
  '';

  environment.systemPackages = with pkgs; [
    claude-code
    direnv
    gh
    gitFull
    jq
    ripgrep
    tmux
  ];
}
