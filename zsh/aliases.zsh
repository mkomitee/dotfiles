#!/usr/bin/env zsh
# Simple stats

alias bar_chart='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/bar_chart.py'
alias histogram='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/histogram.py'
alias ninety_five_percent='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/ninety_five_percent.py'
alias run_for='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/run_for.py'
alias sample='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/sample.py'

alias aa='tmux_resume main'
alias bc='bc -l'
alias dot='cd ~dot'
alias tmux='tmux -u2'
alias y=true

alias locate.home='locate -d ~/.locate.home'

alias type='type -a'
alias dirs='dirs -v'

alias ag=rg

alias l='exa -gbFs date --group-directories-first'
alias la='l --all'
alias ll='l --long'
alias lla='ll --all'
alias llg='ll --git'
alias llga='lla --git'
alias llag='llga'

# Vim w/ vim remote stuff, unfortunately neovim doesn't protect the socket file
# w/ unix permissions :/
NVIM_LISTEN_ADDRESS=/tmp/.komitee-nvimsocket/sock
export NVIM_LISTEN_ADDRESS
if [ ! -d $(dirname $NVIM_LISTEN_ADDRESS) ]; then
    omask=$(umask)
    umask 077
    mkdir -p $(dirname $NVIM_LISTEN_ADDRESS)
    umask ${omask}
fi
if [ -e $NVIM_LISTEN_ADDRESS ]; then
    chmod 700 $NVIM_LISTEN_ADDRESS
fi

alias vim=nvim
alias vi=nvim
alias view="nvim -R"
alias e='nvr -s'
alias eb='nvr --remote-wait -s'

alias git=hub
alias less='less -RS'

alias cat=bat

alias bc=eva
