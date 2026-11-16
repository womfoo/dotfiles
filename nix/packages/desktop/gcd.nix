{
  pkgs,
  writeScript,
  ...
}:
writeScript {
  name = "gcd";
  runtimeInputs = with pkgs; [
    dmenu
    findutils
    pick
  ];
  text = ''
    PICKER=
    # dumb when inside X
    if [ "$TERM" = 'dumb' ]; then
      PICKER='dmenu -l 30'
    else
      PICKER='pick'
    fi
    GIT_BASE_DIR="$HOME/git"
    GIT_REPO=$(find "$GIT_BASE_DIR" -maxdepth 4 -name .git -exec dirname {} \; | $PICKER )
    cd "$GIT_REPO"
    if [ "$TERM" = 'dumb' ]; then
      st tmux
    else
      bash
    fi
  '';
}
