{
  pkgs,
}:
let
  prog = ./gco.py;
in
pkgs.writeScriptBin "gco" ''
  exec ${pkgs.python3}/bin/python ${prog} "$@"
''
