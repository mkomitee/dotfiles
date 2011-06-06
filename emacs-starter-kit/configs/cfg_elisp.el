(add-hook 'lisp-mode-hook 'whitespace-mode)
(add-hook 'lisp-mode-hook 'flymake-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'lisp-mode-hook
          (let ((original-command 'lisp-indent-line))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
              (local-set-key [tab] 'yas/expand))))

(require 'highlight-80+)
(add-hook 'lisp-mode-hook
          '(lambda () (highlight-80+-mode 1)) t)

(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'emacs-lisp-mode-hook
          (let ((original-command 'emacs-lisp-indent-line))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
              (local-set-key [tab] 'yas/expand))))

(require 'highlight-80+)
(add-hook 'emacs-lisp-mode-hook
          '(lambda () (highlight-80+-mode 1)) t)
