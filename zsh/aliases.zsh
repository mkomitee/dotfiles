#!/bin/zsh
alias sudo='nocorrect sudo'
alias grep='nocorrect egrep --color'
alias rm='nocorrect rm'
alias rmdir='nocorrect rmdir'
alias mv='nocorrect mv'
alias mkdir='nocorrect mkdir'

alias bc='bc -l'
alias s=sudo
alias history='fc -dl 1'
alias sudoenv='sudo -E zsh'
alias tmux='tmux -u'
alias t='tasks list -v'
if [ -f /usr/bin/vimx ]; then
    alias vim=vimx
fi
