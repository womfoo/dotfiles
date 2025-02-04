{
  pkgs,
  writeScript,
  ...
}:
writeScript {
  name = "sendkeys";
  runtimeInputs = with pkgs; [
    hiera-eyaml
    oathToolkit
    xdotool
    yq
  ];
  text = builtins.readFile ./sendkeys.sh;
}
