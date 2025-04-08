{
  pkgs,
}:
let
  prog = ./mvb2sum.py;
in
pkgs.writeScriptBin "mvb2sum" ''
  exec ${pkgs.python3}/bin/python ${prog} "$@"
''
