(add-hook 'perl-mode-hook 'whitespace-mode)
(add-hook 'perl-mode-hook 'flymake-mode)
(add-hook 'perl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'perl-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
