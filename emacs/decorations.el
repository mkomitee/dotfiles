(req-package linum
  :config (global-linum-mode t)
  )

;; If we have a fringe, make it 8 pixels wide
(if (featurep 'fringe)
    (fringe-mode 8))

(req-package moe-theme
  :config (load-theme 'moe-dark t))

;; molokai-theme
