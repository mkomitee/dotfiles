#!/usr/bin/env python
"""
Ensure my home directory is properly configured
"""

from __future__ import print_function

import os
import os.path
import sys

LINKS = (
    ("~/.zshenv", ".dotfiles/zshenv"),
    ("~/.zshrc", ".dotfiles/zshrc"),
    ("~/.ctags", ".dotfiles/ctags"),
    ("~/.editrc", ".dotfiles/editrc"),
    ("~/.inputrc", ".dotfiles/inputrc"),
    ("~/.tmux.conf", ".dotfiles/tmux/tmux.conf"),
    ("~/.vim", ".dotfiles/vim"),
    ("~/scripts", ".dotfiles/scripts"),
    ("~/.xmonad/xmonad.hs", "../.dotfiles/xmonad.hs"),
    ("~/.xmobarrc", ".dotfiles/xmobarrc"),
)

CONTAINS = (
    ("~/.vimrc", "source $HOME/.dotfiles/vim/vimrc"),
    ("~/.gvimrc", "source $HOME/.dotfiles/vim/gvimrc"),
    ("~/.gitconfig", "path = .dotfiles/gitconfig"),
)


def check_link(source, destination):
    """Print an error message if the source isn't a link to the destination"""
    try:
        source = os.path.expanduser(source)
        if os.readlink(source) != destination:
            print("{0} does not point to {1}".format(source, destination),
                  file=sys.stderr)
            return False
    except (OSError,):
        print("{0} does not point to {1}".format(source, destination),
              file=sys.stderr)
        return False
    else:
        return True


def check_contains(path, substring):
    """Print an error message if file at path doesn't contain the substring"""
    try:
        path = os.path.expanduser(path)
        with open(path, 'r') as fhandle:
            if not substring in fhandle.read():
                print("{0} does not contain {1}".format(path, substring),
                      file=sys.stderr)
                return False
    except (OSError,):
        print("{0} does not contain {1}".format(path, substring),
              file=sys.stderr)
        return False
    else:
        return True


def main():
    """Run all checks"""
    result = True
    for source, destination in LINKS:
        if not check_link(source, destination):
            result = False
    for path, substring in CONTAINS:
        if not check_contains(path, substring):
            result = False
    if result:
        return 0
    else:
        return 1


if __name__ == '__main__':
    exit(main())
