let
  inherit (inputs.nixpkgs) lib;
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
          age.secrets."${baseName}".file = ./${name};
        };
      }
    ) ageFiles
  );
in
ageAttrs
