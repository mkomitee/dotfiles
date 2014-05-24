(add-hook 'after-init-hook (sml/setup))

(global-pretty-mode)

(after 'diminish-autoloads
  (diminish 'global-visual-line-mode)
  (diminish 'visual-line-mode)
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
  (after 'git-gutter+ (diminish 'git-gutter+-mode)))

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

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "snow"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "light slate blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "honeydew"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red")))))


;; Causes god mode to change the cursor from a pipe to a box
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
