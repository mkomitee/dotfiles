
(defvar komitee/packages '(ace-window
                           company
                           company-anaconda
                           company-go
                           diminish
                           elisp-slime-nav
                           evil
                           evil-args
                           evil-commentary
                           evil-easymotion
                           evil-indent-textobject
                           evil-jumper
                           evil-leader
                           evil-matchit
                           evil-snipe
                           evil-surround
                           evil-tabs
                           evil-visualstar
                           exec-path-from-shell
                           expand-region
                           fill-column-indicator
                           flycheck
                           go-mode
                           haskell-mode
                           markdown-mode+
                           moe-theme
                           molokai-theme
                           popwin
                           projectile
                           rainbow-delimiters
                           undo-tree
                           use-package))


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

(require 'use-package)
