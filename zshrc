# Path to your oh-my-zsh configuration.

export ZSH=$HOME/dotfiles/oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="komitee"

# Set to this to use case-sensitive completion
export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# List of my usernames (needed for my prompt theme)
usernames=(komitee mkomitee zim)

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(pathrc vi-mode git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

export PYTHONPATH="$HOME/dotfiles/lib/python"

. ~/.zshrc.local
