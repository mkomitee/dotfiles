#!/bin/zsh
alias sudo='nocorrect sudo'
alias grep='nocorrect egrep --color'
alias rm='nocorrect rm'
alias rmdir='nocorrect rmdir'
alias mv='nocorrect mv'
alias mkdir='nocorrect mkdir'

alias git=hub
alias gp='git pull'
alias gaa='git add -A'

alias mutt='ulimit -n 10240;mutt'
alias bc='bc -l'
alias s=sudo
alias sudoenv='sudo -E zsh'
alias tmux='tmux -u2'
alias t='tasks list -v'

if [ -f /usr/bin/vimx ]; then
    alias vim=vimx
fi
alias vi=vim
