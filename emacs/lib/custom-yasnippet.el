(defvar yas-fallback-behavior 'return-nil)
(defvar yas-also-auto-indent-first-line t)

(require-package 'yasnippet)
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.dotfiles/emacs/snippets"))
(yas-global-mode t)

(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map [tab] nil)

(provide 'custom-yasnippet)
