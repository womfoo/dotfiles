{
  pkgs,
  writeScript,
  ...
}:
writeScript {
  name = "backup";
  runtimeInputs = with pkgs; [
    rsync
  ];
  text = ./backup.sh;
}
