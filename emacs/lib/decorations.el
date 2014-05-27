(add-hook 'after-init-hook (sml/setup))

(global-pretty-mode)

(after 'diminish-autoloads
  (diminish 'global-visual-line-mode)
  (diminish 'visual-line-mode)
  (after 'flyspell (diminish 'flyspell-mode))
  (after 'simple (diminish 'auto-fill-function))
  (after 'paredit (diminish 'paredit-mode))
  (after 'hi-lock (diminish 'hi-lock-mode))
  (after 'flycheck (diminish 'flycheck-mode))
  (after 'visual-line (diminish 'visual-line-mode))
  (after 'undo-tree (diminish 'undo-tree-mode))
  (after 'auto-complete (diminish 'auto-complete-mode))
  (after 'projectile (diminish 'projectile-mode))
  (after 'guide-key (diminish 'guide-key-mode))
  (after 'eldoc (diminish 'eldoc-mode))
  (after 'smartparens (diminish 'smartparens-mode))
  (after 'company (diminish 'company-mode))
  (after 'whitespace (diminish 'whitespace-mode))
  (after 'git-gutter+ (diminish 'git-gutter+-mode))
  (after 'writegood-mode (diminish 'writegood-mode))
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
(setq custom-safe-themes '("0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" default)
      frame-background-mode 'dark)
(require 'molokai-theme)
(load-theme 'molokai)

;; Causes god mode to change the cursor from a pipe to a box
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
