# Path to your oh-my-zsh configuration.
ZSH=$HOME/.dotfiles/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="komitee-simple"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(pathrc git vi-mode terminalapp)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
# Unset options we don't want which were set in lib/plugins {{{1
unsetopt auto_cd
unsetopt cdablevarS
unsetopt complete_in_word
unsetopt share_history
unsetopt SHARE_HISTORY

# Set options we do want {{{1
setopt nocorrect_all

# Remove aliases we don't want which are defined in oh-my-zsh/lib and activated plugins {{{1
unalias pu
unalias po
unalias ...
unalias -- -
unalias _
unalias lsa
unalias l
unalias ll
unalias sl
unalias heroku
unalias ebuild
unalias hpodder
unalias ..
unalias 1
unalias 2
unalias 3
unalias 4
unalias 5
unalias 6
unalias 7
unalias 8
unalias 9
unalias cd..
unalias cd...
unalias cd....
unalias cd.....
unalias cd/
unalias gist
unalias md
unalias mysql
unalias rd
unalias run-help

# Also remove defined functions we dont want {{{1
unfunction cd
unfunction mcd

# Add our own aliases {{{1
alias WINCH='kill -WINCH $$'
alias bash='env XTERM_LEVEL=$((XTERM_LEVEL+1)) bash'
alias bc='bc -l'
alias grep='nocorrect egrep --color'
alias s=sudo
alias svnkeywords="svn propset svn:keywords 'Author HeadURL Id Revision URL Date'"
alias today='date +%Y/%m/%d'
alias afind='ack -il'
alias dirs='dirs -v'
alias rm='nocorrect rm'
alias rmdir='nocorrect rmdir'
alias history='fc -dl 1'
alias sudoenv='sudo -E zsh'
alias tasks='t --task-dir=~/.tasks --list=tasks'
alias task='t --task-dir=~/.tasks --list=tasks'
alias t='t --task-dir=~/.tasks --list=tasks'
alias tmux='tmux -u'

# Tweak environment variables {{{1
# LSCOLORS="Exfxcxdxbxegedabagacad"
