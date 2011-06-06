(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'flymake-mode)
(add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ruby-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)
;; (require 'ruby-block)
;; (ruby-block-mode t)

