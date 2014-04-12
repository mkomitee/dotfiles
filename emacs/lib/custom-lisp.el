(require-package 'rainbow-delimiters)
(require-package 'paredit)

(defun my-lisp-hook ()
  (progn
    (turn-on-eldoc-mode)
    (require 'evil-paredit)
    (paredit-mode t)
    (evil-paredit-mode t)
    (rainbow-delimiters-mode t)))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
(add-hook 'ielm-mode-hook 'my-lisp-hook)

(provide 'custom-lisp)
