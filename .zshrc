# emacs/tramp fails without this
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='> '
export PATH=/nix/var/nix/profiles/system/sw/bin:$PATH
eval "$(direnv hook zsh)"
eval "$(z --init zsh)"
