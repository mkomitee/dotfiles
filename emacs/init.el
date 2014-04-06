;; add our additional libraries to our load path
(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.dotfiles/emacs/lib")
(add-to-list 'load-path "~/.dotfiles/emacs/config")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)


;; Load local modifications
(require 'local-pre)

(require 'custom-utils)
(require 'custom-core)
(require 'custom-decorations)
(require 'custom-smartparens)
(require 'custom-yasnippet)
(require 'custom-autocomplete)
(require 'custom-projectfile)
(require 'custom-helm)
(require 'custom-ido)
(require 'custom-git)
(require 'custom-flycheck)
(require 'custom-vim)
(require 'custom-lisp)
(require 'custom-markdown)
(require 'custom-ag)
(require 'custom-ace)
(require 'custom-evil)
(require 'custom-maps)
(require 'custom-python)
(require 'custom-haskell)
(require 'custom-puppet)

;; Load local modifications
(require 'local-post)

;; If we're in a window system of any kind
(if window-system
    ;; start the server
    (server-start))
