#!/bin/zsh
alias sudo='nocorrect sudo'
alias rm='nocorrect rm'
alias rmdir='nocorrect rmdir'
alias mv='nocorrect mv'
alias mkdir='nocorrect mkdir'

alias gp='git pull'
alias gaa='git add -A'

alias mutt='ulimit -n 10240;mutt'
alias bc='bc -l'
alias s=sudo
alias sudoenv='sudo -E zsh'
alias s-s='sudo -s'
alias tmux='tmux -u2'

alias pomodoro='pomodoro.py'
alias code-notes='ack "TODO|FIXME"'
alias dot='cd ~dot'

alias vi=vim

alias ack='noglob ack'
