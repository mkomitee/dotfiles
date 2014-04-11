(require-package 'smart-mode-line)
(add-hook 'after-init-hook (sml/setup))

(require-package 'pretty-mode)
(global-pretty-mode)

(require-package 'smooth-scroll)
(require 'smooth-scroll)
(setq smooth-scroll/vscroll-step-size 8)
(smooth-scroll-mode t)

(require-package 'fill-column-indicator)
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkred")
(setq fci-rule-character ?\u254e)

(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq fci-rule-column 80)))

(require-package 'diminish)
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
  (after 'git-gutter+ (diminish 'git-gutter+-mode))
  (after 'pretty-mode (diminish 'smooth-scroll-mode)))

(global-hl-line-mode +1)

(require 'linum)
(setq linum-format "%4d ")
(global-linum-mode t)

;; If we don't have a fringe, add a pipe to separate line numbers from
;; our text
(if (not (featurep 'fringe))
  (setq linum-format "%d \u254e"))

;; Theming, ...
(setq frame-background-mode 'dark)
(require-package 'molokai-theme)
(require 'molokai-theme)
(load-theme 'molokai)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))



(provide 'custom-decorations)
