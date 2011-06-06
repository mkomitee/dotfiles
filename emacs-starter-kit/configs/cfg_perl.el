(add-hook 'perl-mode-hook 'whitespace-mode)
(add-hook 'perl-mode-hook 'flymake-mode)
(add-hook 'perl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'perl-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'perl-mode-hook
          (let ((original-command (viper-nil)))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
               (local-set-key [tab] 'yas/expand))))
