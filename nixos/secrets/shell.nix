with import <nixpkgs> {};
let
  sops-nix = import /home/kranium/git/github.com/Mic92/sops-nix;
in
mkShell {
  sopsPGPKeyDirs = [
    "./keys/hosts"
    "./keys/users"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage sops-nix {}).sops-import-keys-hook
  ];
}
