(defun komitee/prose-hook ()
  (progn
    (turn-on-auto-fill)
    (diminish 'auto-fill-function)
    (set-fill-column 72)
    (turn-on-fci-mode)
    (column-number-mode 1)
    (flyspell-mode)
    )
  )

(add-hook 'text-mode-hook 'komitee/prose-hook)
(add-hook 'rst-mode-hook 'komitee/prose-hook)

(req-package markdown-mode
  :config (add-hook 'markdown-mode-hook 'komitee/prose-hook)
  )
