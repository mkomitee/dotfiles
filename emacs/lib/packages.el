(defvar komitee/packages '(ace-jump-mode
                           ace-window
                           ag
                           auto-complete
                           better-defaults
                           diminish
                           elpy
                           exec-path-from-shell
                           fill-column-indicator
                           flx-ido
                           flycheck
                           git-gutter+
                           git-gutter-fringe+
                           god-mode
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
