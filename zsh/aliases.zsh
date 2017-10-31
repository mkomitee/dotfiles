#!/usr/bin/env zsh
# Simple stats
alias bar_chart='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/bar_chart.py'
alias histogram='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/histogram.py'
alias ninety_five_percent='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/ninety_five_percent.py'
alias run_for='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/run_for.py'
alias sample='python ~/.dotfiles/contrib/bitly/data_hacks.git/data_hacks/sample.py'

# git aliases
alias gaa="git add --all ."
alias gap="git add --patch"
alias gcv="git commit -v"
alias gd="git diff"
alias gdc="git diff --cached"
alias gds="git diff --staged"
alias gs="git status --short --branch"

alias vi=vim
alias aa='tmux_resume main'
alias ack='ag'
alias bc='bc -l'
alias dot='cd ~dot'
alias lss=less
alias reexec="exec zsh -l"
alias s-s='sudo -s'
alias tig='tig --all'
alias tmux='tmux -u2'
alias y=true

alias e="command emacsclient -a emacs -q -n"
alias ex="command emacsclient -a emacs -q -n"
alias exw="command emacsclient -a emacs -q"
alias ec="command emacsclient -a emacs -q -t"
alias ecw="command emacsclient -a emacs -q -t"

alias type='type -a'

alias pyprofile='python -mcProfile -o $(uuid -v4).profile'

alias locate.home='locate -d ~/.locate.database'

alias dirs='dirs -v'

alias -g H='head'
alias -g T='tail'
alias -g L='less'

alias fgls='git ls-files | fzf -i -e'

alias ag=rg
