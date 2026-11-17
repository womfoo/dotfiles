# claudebox

A lightweight NixOS [microvm](https://github.com/microvm-nix/microvm.nix) sandbox
for coding agents such as Claude Code. Only the repos you list are visible
inside it. Nothing else from the host is shared: no SSH keys, no `~/.config`,
no agenix secrets, no `lihim`.

## Quick start

1. Add your SSH public key to `users.users.claude.openssh.authorizedKeys.keys`
   in `default.nix`.
2. Boot it. The VM stays in the foreground, so use a separate terminal or tmux:

   ```sh
   cd ~/vms/claudebox    # any directory; disk images are created here
   nix run ~/git/github.com/womfoo/dotfiles#claudebox
   ```

3. Log in:

   ```sh
   ssh -p 2232 claude@localhost
   ```

To stop it, run `sudo poweroff` inside the VM, or press `Ctrl-a x` on the console.

## What's inside

| Thing        | Value                                                     |
|--------------|-----------------------------------------------------------|
| Hypervisor   | QEMU, 4 vCPUs, 8 GiB RAM                                  |
| Network      | User-mode NAT (no root or tap on the host)                |
| SSH          | `127.0.0.1:2232` → guest `:22`, key-only, no root login   |
| User         | `claude`, uid 2000, passwordless sudo inside the VM only  |
| Tools        | `claude-code`, `gh`, `git`, `direnv`, `ripgrep`, `jq`, `tmux` |
| Nix          | Flakes on, host `/nix/store` shared read-only, writable overlay |

## Repos

Edit the `repos` list at the top of `default.nix`. Each path is mounted
read-write over 9p at the **same path** inside the VM, so git remotes, flake
paths and error messages match the host.

`lihim` is left out on purpose because it holds real secrets.

## State

These images are created in whatever directory you `nix run` from:

| Image                | Mount            | Size   |
|----------------------|------------------|--------|
| `claudebox-home.img` | `/home/claude`   | 20 GiB |
| `claudebox-nix.img`  | `/nix/.rw-store` | 40 GiB |

Claude Code login, `gh` auth and shell history live in the home image. To
reset the VM, delete both images.

## Isolation notes

- **File ownership.** 9p accesses files as the host user running QEMU. When
  you run the VM as `kranium`, changes in the repos are owned by you. That's
  why the guest user also has uid 2000.
- **Running it under its own host account.** Create a dedicated host user
  with write access to the listed repos only, then either run the VM as that
  user, or move to `microvm.nixosModules.host` on vhagar (a systemd service,
  virtiofs instead of 9p).
- **Credentials.** Give the agent its own credentials: a fine-grained GitHub
  token scoped to these repos, plus a deploy key if it needs to push. Don't
  share your personal key or `~/.config/gh`.
- **Network.** User-mode NAT still gives the VM full outbound internet. To
  restrict it, switch to a tap interface on vhagar with firewall rules.
