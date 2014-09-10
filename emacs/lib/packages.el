(defvar komitee/packages '(ace-jump-mode
                           ace-window
                           ag
                           better-defaults
                           diminish
                           evil
                           evil-indent-textobject
                           evil-leader
                           evil-matchit
                           evil-nerd-commenter
                           evil-tabs
                           evil-visualstar
                           exec-path-from-shell
                           expand-region
                           fill-column-indicator
                           flx-ido
                           go-mode
                           guide-key
                           ido-hacks
                           ido-ubiquitous
                           ido-vertical-mode
                           markdown-mode+
                           molokai-theme
                           popwin
                           projectile
                           rainbow-delimiters
                           smex
                           surround
                           undo-tree
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
