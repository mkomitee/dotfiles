#!/usr/bin/env bash

# Integrate copy/paste w/ the x & macos clipboards.
tmux unbind-key -T copy-mode-vi 'y'
tmux unbind p
if [ "$(uname)" = 'Linux' ]; then
    tmux bind-key -T copy-mode-vi 'y' send -X copy-pipe "xclip -i"
    tmux bind-key p run-shell 'tmux set-buffer "$(xclip -o)"; tmux paste-buffer'
else
    tmux bind-key -T copy-mode-vi 'y' send -X copy-pipe "pbcopy"
    tmux bind-key p run-shell 'tmux set-buffer "$(pbpaste)"; tmux paste-buffer'
fi
