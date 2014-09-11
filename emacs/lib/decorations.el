(after 'diminish-autoloads
  (diminish 'global-visual-line-mode)
  (diminish 'visual-line-mode)
  (after 'flyspell (diminish 'flyspell-mode))
  (after 'simple (diminish 'auto-fill-function))
  (after 'hi-lock (diminish 'hi-lock-mode))
  (after 'visual-line (diminish 'visual-line-mode))
  (after 'undo-tree (diminish 'undo-tree-mode))
  (after 'projectile (diminish 'projectile-mode))
  (after 'guide-key (diminish 'guide-key-mode))
  (after 'eldoc (diminish 'eldoc-mode))
  (after 'smartparens (diminish 'smartparens-mode))
  (after 'whitespace (diminish 'whitespace-mode))
  (after 'company (diminish 'company-mode))
  (after 'yasnippet (diminish 'yas-minor-mode))
  )

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
(require 'molokai-theme)
(load-theme 'molokai)
