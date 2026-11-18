{
  pkgs,
}:
let
  prog = ./gitshuffle.py;
in
pkgs.writeShellScriptBin "gitshuffle" ''
  export PATH=${pkgs.lib.makeBinPath [ pkgs.git ]}:$PATH
  exec ${pkgs.python3}/bin/python ${prog} "$@"
''
