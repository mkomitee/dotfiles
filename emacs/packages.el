(defvar komitee/packages '(ace-jump-mode
                           ace-window
                           ag
                           better-defaults
                           company
                           company-go
                           company-anaconda
                           diminish
                           elisp-slime-nav
                           evil
                           evil-indent-textobject
                           evil-jumper
                           evil-leader
                           evil-matchit
                           evil-nerd-commenter
                           evil-tabs
                           evil-visualstar
                           exec-path-from-shell
                           expand-region
                           fill-column-indicator
                           flycheck
                           flx-ido
                           go-mode
                           guide-key
                           haskell-mode
                           ido-hacks
                           ido-ubiquitous
                           ido-vertical-mode
                           markdown-mode+
                           moe-theme
                           molokai-theme
                           popwin
                           projectile
                           puppet-mode
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
