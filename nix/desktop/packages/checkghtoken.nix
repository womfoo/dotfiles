{
  pkgs,
}:
let
  python' = (
    pkgs.python3.withPackages (
      ps:
      with ps;
      with pkgs.python3Packages;
      [
        requests
        pytz
      ]
    )
  );
  prog = ./checkghtoken.py;
in
pkgs.writeScriptBin "checkghtoken" ''
  exec ${python'}/bin/python ${prog}
''
