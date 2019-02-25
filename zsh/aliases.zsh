#!/usr/bin/env zsh
# Simple stats

alias bar_chart='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/bar_chart.py'
alias histogram='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/histogram.py'
alias ninety_five_percent='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/ninety_five_percent.py'
alias run_for='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/run_for.py'
alias sample='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/sample.py'

alias vi=vim
alias aa='tmux_resume main'
alias bc='bc -l'
alias dot='cd ~dot'
alias tmux='tmux -u2'
alias y=true

if [  -z "${VIM_SERVERNAME}" ]; then
    VIM_SERVERNAME="VIM"
fi

alias e="command vim --servername $VIM_SERVERNAME --remote"
alias ec="command vim --servername $VIM_SERVERNAME --remote"
alias ecw="command vim --servername $VIM_SERVERNAME --remote-wait"
alias ex="command gvim --servername $VIM_SERVERNAME --remote"
alias exw="command gvim --servername $VIM_SERVERNAME --remote-wait"

alias type='type -a'

alias locate.home='locate -d ~/.locate.home'

alias dirs='dirs -v'
