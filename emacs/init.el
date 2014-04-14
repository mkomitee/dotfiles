;; add our additional libraries to our load path
(add-to-list 'load-path "~/.dotfiles/emacs/lib")

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'custom-utils)
(require 'custom-core)
(require 'custom-evil)
(require 'custom-decorations)
(require 'custom-yasnippet)
(require 'custom-autocomplete)
(require 'custom-projectfile)
(require 'custom-ido)
(require 'custom-git)
(require 'custom-flycheck)
(require 'custom-vim)
(require 'custom-lisp)
(require 'custom-markdown)
(require 'custom-ag)
(require 'custom-ace)
(require 'custom-python)
(require 'custom-make)
(require 'custom-haskell)
(require 'custom-puppet)
(require 'custom-winner)
(require 'custom-maps)

;; If we're in a window system of any kind
(if window-system
    ;; start the server
    (server-start))
