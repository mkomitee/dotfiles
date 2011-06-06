;; (add-hook 'cperl-mode-hook 'whitespace-mode)
;; (add-hook 'cperl-mode-hook 'flymake-mode)
;; (add-hook 'cperl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'cperl-mode-hook
          (let ((original-command 'cperl-indent-line))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
              (local-set-key [tab] 'yas/expand))))

;; (require 'highlight-80+)
;; (add-hook 'cperl-mode-hook
;;           '(lambda () (highlight-80+-mode 1)) t)
