(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-indent-level 4)))
(add-hook 'ruby-mode-hook 'turn-on-fic-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'ruby-mode-hook
          (let ((original-command 'ruby-indent-line))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
               (local-set-key [tab] 'yas/expand))))
(add-hook 'ruby-mode-hook
          (lambda ()
            (ruby-electric-mode)))
(add-hook 'ruby-mode-hook
          '(lambda () (highlight-80+-mode 1)) t)

;; Experimental
;; (require 'ruby-block)
;; (ruby-block-mode t)

