{
  pkgs,
  writeScript,
  ...
}:
writeScript {
  name = "sendkeys";
  runtimeInputs =
    with pkgs;
    [
      hiera-eyaml
      oathToolkit
      yq
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.xdotool ];

  text = builtins.readFile ./sendkeys.sh;
}
