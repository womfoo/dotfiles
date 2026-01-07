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
      oath-toolkit
      yq
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.xdotool ];

  text = builtins.readFile ./sendkeys.sh;
}
