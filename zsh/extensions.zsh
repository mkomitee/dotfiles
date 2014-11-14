#!/usr/bin/env zsh

# quote pasted URLs
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
