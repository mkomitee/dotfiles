(require 'linum)
(setq linum-format "%4d ")
(global-linum-mode t)

;; If we have a fringe, make them16 pixels wide, otherwise add a pipe
;; symbol to separate line numbers from our text
(if (featurep 'fringe)
    (fringe-mode 8)
  (setq linum-format "%d \u254e"))

;; Theming, ...
(setq frame-background-mode 'dark)

(req-package moe-theme
  :config (load-theme 'moe-dark t))

;; molokai-theme
