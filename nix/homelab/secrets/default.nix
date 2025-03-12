let
  inherit (inputs.nixpkgs) lib;
  inherit (inputs.lihim.pubkeys.constants) admins machines;
  dir = ./.;
  files = builtins.readDir dir;
  ageFiles = lib.attrNames (lib.filterAttrs (name: value: lib.hasSuffix ".age" name) files);
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
    "nix/homelab/secrets/wg-${host}-priv-key.age".publicKeys = [
      admins.kranium
      admins.kraniumarm
      machines."${host}"
    ];
  };
  # FIXME: organise as std blockType?
  ageConfig.secrets-nix =
    {
      "nix/homelab/secrets/init-root-password.age".publicKeys = [
        admins.kranium
        admins.kraniumarm
      ] ++ builtins.attrValues machines;
      "nix/homelab/secrets/builder-key.age".publicKeys = [
        admins.kranium
        admins.kraniumarm
      ] ++ builtins.attrValues machines;
    }
    // mkWireguardHostKey "stonedoor"
    // mkWireguardHostKey "waycastle";

in
ageAttrs // ageConfig
