(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode t)

(evil-define-key 'insert yas-minor-mode-map (kbd "TAB")
  'yas-next-field-or-maybe-expand)

(provide 'custom-yasnippet)
