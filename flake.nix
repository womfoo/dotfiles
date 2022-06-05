{
  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/9ff65cb28c43236eac4cbbd20a5781581d9cbbf6";
    std.url = "github:divnix/std";
  };

  outputs = inputs:
    (inputs.std.growOn {
      inherit inputs;
      cellsFrom = ./cells;
      organelles = [
        (inputs.std.devshells "devshell")
      ];
    }
    {
      devShell = inputs.std.harvest inputs.self [ "automation" "devshell" "default" ];
    });
}
