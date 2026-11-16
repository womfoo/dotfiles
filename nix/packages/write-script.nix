# Same output as divnix/std's lib.ops.writeScript, so derivations are unchanged.
{ pkgs }:
{
  name,
  text,
  runtimeInputs ? [ ],
  runtimeEnv ? { },
  runtimeShell ? pkgs.runtimeShell,
  checkPhase ? null,
}:
let
  inherit (pkgs) lib;
  runtimeShell' = if runtimeShell != pkgs.runtimeShell then lib.getExe runtimeShell else runtimeShell;
in
pkgs.writeTextFile {
  inherit name;
  executable = true;
  destination = "/bin/${name}";
  text = ''
    #!${runtimeShell'}
    # shellcheck shell=bash
    set -o errexit
    set -o pipefail
    set -o nounset
    set -o functrace
    set -o errtrace
    set -o monitor
    set -o posix
    shopt -s dotglob

  ''
  + lib.optionalString (runtimeInputs != [ ]) ''
    export PATH="${lib.makeBinPath runtimeInputs}:$PATH"
  ''
  + lib.optionalString (runtimeEnv != { }) ''
    ${lib.concatStringsSep "\n" (
      lib.mapAttrsToList (n: v: "export ${n}=${''"$''}{${n}:-${toString v}}${''"''}") runtimeEnv
    )}
  ''
  + ''

    ${text}
  '';
  checkPhase =
    if checkPhase == null then
      ''
        runHook preCheck
        ${pkgs.stdenv.shellDryRun} "$target"
        ${pkgs.shellcheck}/bin/shellcheck "$target"
        runHook postCheck
      ''
    else
      checkPhase;
  meta.mainProgram = name;
}
