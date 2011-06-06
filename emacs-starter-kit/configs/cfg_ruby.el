(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'flymake-mode)
(add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'ruby-mode-hook
          (let ((original-command 'ruby-indent-line))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
              (local-set-key [tab] 'yas/expand))))

(require 'highlight-80+)
(add-hook 'ruby-mode-hook
          '(lambda () (highlight-80+-mode 1)) t)

;;; Experimental

;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)
;; (require 'ruby-block)
;; (ruby-block-mode t)

