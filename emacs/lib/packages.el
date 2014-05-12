(defvar komitee/packages '(ace-jump-mode
                           ace-window
                           ag
                           auto-complete
                           diminish
                           exec-path-from-shell
                           fill-column-indicator
                           flx-ido
                           flx-ido
                           flycheck
                           gist
                           git-gutter+
                           git-gutter-fringe+
                           god-mode
                           guide-key
                           haskell-mode
                           ido-ubiquitous
                           ido-ubiquitous
                           ido-vertical-mode
                           ido-vertical-mode
                           jedi
                           jedi-direx
                           magit
                           markdown-mode
                           molokai-theme
                           multiple-cursors
                           paredit
                           popwin
                           pretty-mode
                           projectile
                           puppet-mode
                           python
                           rainbow-delimiters
                           smart-mode-line
                           smex
                           smex
                           surround
                           undo-tree
                           vimrc-mode
                           yasnippet))


(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package komitee/packages)
  (unless (package-installed-p package)
    (package-install package)))
