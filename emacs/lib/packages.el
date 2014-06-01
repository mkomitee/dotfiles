(defvar komitee/packages '(ace-jump-mode
                           ace-window
                           ag
                           auto-complete
                           better-defaults
                           diminish
                           elpy
                           evil
                           evil-indent-textobject
                           evil-leader
                           evil-matchit
                           evil-nerd-commenter
                           evil-paredit
                           evil-visualstar
                           exec-path-from-shell
                           expand-region
                           fill-column-indicator
                           flx-ido
                           flycheck
                           git-gutter+
                           git-gutter-fringe+
                           guide-key
                           haskell-mode
                           ido-hacks
                           ido-ubiquitous
                           ido-vertical-mode
                           magit
                           markdown-mode
                           molokai-theme
                           multiple-cursors
                           paredit
                           popwin
                           pretty-mode
                           projectile
                           puppet-mode
                           rainbow-delimiters
                           smart-mode-line
                           smex
                           surround
                           undo-tree
                           vimrc-mode
                           writegood-mode
                           yasnippet))


(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package komitee/packages)
  (unless (package-installed-p package)
    (package-install package)))
