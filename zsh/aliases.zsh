#!/bin/zsh
alias tig='tig --all'
alias bc='bc -l'
alias s-s='sudo -s'
alias tmux='tmux -u2'
alias dot='cd ~dot'
alias ack='noglob ack'
alias a='tmux -u2 attach-session -t main || tmux -u2  new-session -s main'
alias turses='(source ~/.venv/turses/bin/activate && turses -a mkomitee)'
alias -g bar_chart='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/bar_chart.py'
alias -g histogram='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/histogram.py'
alias -g ninety_five_percent='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/ninety_five_percent.py'
alias -g run_for='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/run_for.py'
alias -g sample='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/sample.py'
alias -g vi=vim
alias lss=less
alias boom='ruby ~/.dotfiles/contrib/holman/boom/bin/boom'
