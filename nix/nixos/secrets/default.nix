# agenix secrets: every ./<name>.age becomes
#   secrets.<name>.nixosModule  - declares age.secrets.<name>
#   secrets.<name>.path config  - the decrypted path on the host
# plus `secrets-nix`, the ragenix rules (see gen-secrets-nix).
{
  lib,
  admins,
  machines,
}:
let
  dir = "nix/nixos/secrets";
  files = builtins.readDir ./.;
  ageFiles = lib.attrNames (lib.filterAttrs (name: _: lib.hasSuffix ".age" name) files);
  ageAttrs = lib.listToAttrs (
    map (
      name:
      let
        baseName = lib.strings.removeSuffix ".age" name;
      in
      {
        name = baseName;
        value = {
          nixosModule.age.secrets."${baseName}".file = ./${name};
          path = config: config.age.secrets."${baseName}".path;
        };
      }
    ) ageFiles
  );
  mkWireguardHostKey = host: {
    "${dir}/wg-${host}-priv-key.age".publicKeys = [
      admins.kranium
      admins.kraniumarm
      machines."${host}"
    ];
  };
  ageConfig.secrets-nix = {
    "${dir}/init-root-password.age".publicKeys = [
      admins.kranium
      admins.kraniumarm
    ]
    ++ builtins.attrValues machines;
    "${dir}/attic-env.age".publicKeys = [
      admins.kranium
      admins.kraniumarm
      machines.pentos
    ];
    "${dir}/keycloak-db-password.age".publicKeys = [
      admins.kranium
      admins.kraniumarm
      machines.vhagar
    ];
  }
  // mkWireguardHostKey "stonedoor"
  // mkWireguardHostKey "waycastle";

in
ageAttrs // ageConfig
