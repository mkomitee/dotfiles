(defun komitee/prose-hook ()
  (progn
    (writegood-mode)
    (turn-on-auto-fill)
    (set-fill-column 72)
    (turn-on-fci-mode)
    (column-number-mode 1)
    (flyspell-mode)))

(add-hook 'text-mode-hook 'komitee/prose-hook)
(add-hook 'rst-mode-hook 'komitee/prose-hook)
(add-hook 'markdown-mode-hook 'komitee/prose-hook)
