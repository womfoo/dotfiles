export GOPATH=$HOME/go

#export PATH=$PATH:~/.gem/ruby/2.4.0/bin
#export PATH=$PATH:~/bin

# tmux hacks
#export TERM="rxvt-unicode-256color"
#export TERM=xterm-256color

#export PATH=$PATH:$HOME/.cabal/bin/

# append history instead of overwrite
shopt -s histappend
# big history, record everything
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=-1
export HISTFILESIZE=-1

eval "$(direnv hook bash)"
eval "$(z --init bash)"
source <(lace _carapace) # lace-installer
eval "$(direnv hook bash)" # lace-installer
