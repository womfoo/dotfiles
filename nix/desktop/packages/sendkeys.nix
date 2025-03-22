{
  pkgs,
  writeScript,
  eyaml,
  ...
}:
writeScript {
  name = "sendkeys";
  runtimeInputs =
    with pkgs;
    [
      eyaml
      oathToolkit
      yq
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.xdotool ];

  text = builtins.readFile ./sendkeys.sh;
}
