#!/bin/sh
INFO=$(tmux list-panes -F "#S.#I.#P #{pane_active}" | grep "1$"  | cut -d' ' -f1)
printf "#[fg=colour110]$INFO#[default]"
